use std::fmt::{self, Debug};
use std::mem::discriminant;

use crate::frontend::lexer::{Token, TokenType};
use crate::frontend::program_ast::{
    BinaryOperator, BlockItem, CompoundAssignmentOperator, Declaration, Expression,
    FunctionDefinition, ProgramAst, Statement, UnaryOperator,
};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Parse error at line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    current_index: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_index: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<ProgramAst, Vec<ParseError>> {
        match self.parse_function_definition() {
            Ok(function_definition) => {
                if let Err(err) = self.consume(TokenType::EndOfFile, "Expected end of file") {
                    self.errors.push(err);
                }

                if !self.errors.is_empty() {
                    return Err(self.errors);
                }

                Ok(ProgramAst::Program(function_definition))
            }
            Err(err) => {
                // TODO: Synchronize here once there are more cases apart from function definition
                self.errors.push(err);
                Err(self.errors)
            }
        }
    }

    fn parse_function_definition(&mut self) -> Result<FunctionDefinition, ParseError> {
        self.consume(TokenType::IntKeyword, "Expected Int keyword")?;

        let identifier_token = self.consume(
            TokenType::Identifier("".to_string()),
            "Expected function name",
        )?; // The string is a placeholder

        let name = match identifier_token {
            TokenType::Identifier(name) => name,
            _ => unreachable!(), // We already checked that the token is an identifier in the consume method
        };

        self.consume(TokenType::OpenParen, "Expected opened parenthesis")?;
        self.consume(TokenType::VoidKeyword, "Expected void keyword")?;
        self.consume(TokenType::CloseParen, "Expected closed parenthesis")?;
        self.consume(TokenType::OpenBrace, "Expected opened brace")?;

        // Parse function body
        let mut body = Vec::new();

        while self.current_token().token_type != TokenType::CloseBrace
            && self.current_token().token_type != TokenType::EndOfFile
        {
            body.push(self.parse_block_item()?);
        }

        if self.current_token().token_type == TokenType::EndOfFile {
            return Err(ParseError {
                line: self.current_token().line,
                column: self.current_token().column,
                message: "Unexpected end of file while parsing function body".to_string(),
            });
        }

        self.consume(TokenType::CloseBrace, "Expected closed brace")?;

        Ok(FunctionDefinition::Function {
            identifier: name.clone(),
            body: body,
        })
    }

    fn parse_block_item(&mut self) -> Result<BlockItem, ParseError> {
        match self.current_token().token_type {
            TokenType::IntKeyword => {
                return Ok(BlockItem::Declaration(self.parse_declaration()?));
            }
            _ => {
                return Ok(BlockItem::Statement(self.parse_statement()?));
            }
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        self.consume(TokenType::IntKeyword, "Expected Int keyword")?;
        let identifier_token = self.consume(
            TokenType::Identifier("".to_string()),
            "Expected variable name after Type keyword",
        )?; // The string is a placeholder, the consume method checks the variant and not the inner value

        let var_name = match identifier_token {
            TokenType::Identifier(name) => name,
            _ => unreachable!(), // We already checked that the token is an identifier in the consume method
        };

        match self.current_token().token_type {
            TokenType::Semicolon => {
                self.advance();
                Ok(Declaration::VariableDeclaration {
                    identifier: var_name,
                    initial_value: None,
                })
            }
            TokenType::Assignment => {
                self.advance(); // Consume =
                let initial_value = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after variable declaration",
                )?;
                Ok(Declaration::VariableDeclaration {
                    identifier: var_name,
                    initial_value: Some(initial_value),
                })
            }
            _ => Err(ParseError {
                line: self.current_token().line,
                column: self.current_token().column,
                message: "Expected semicolon or assignment after variable name".to_string(),
            }),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token().token_type {
            TokenType::ReturnKeyword => {}
            TokenType::Semicolon => {
                self.advance();
                return Ok(Statement::Null);
            }
            _ => {
                // Parse expressions statement. Only used for expressions with side effects
                let expr = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after expression statement",
                )?;
                return Ok(Statement::Expression(expr));
            }
        }
        self.consume(TokenType::ReturnKeyword, "Expected return keyword")?;
        let return_value = self.parse_expression(0)?;
        self.consume(
            TokenType::Semicolon,
            "Expected semicolon after return statement",
        )?;
        Ok(Statement::Return(return_value))
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let peeked_token = self.current_token();
        match peeked_token.token_type {
            TokenType::IntLiteral(value) => {
                self.advance();
                Ok(Expression::Constant(value))
            }
            // Prefix increment/decrement
            TokenType::Increment => {
                self.advance();
                let inner_expr = self.parse_factor()?;
                Ok(Expression::PrefixIncrement(Box::new(inner_expr)))
            }
            TokenType::Decrement => {
                self.advance();
                let inner_expr = self.parse_factor()?;
                Ok(Expression::PrefixDecrement(Box::new(inner_expr)))
            }
            // Unary operators
            TokenType::Negation | TokenType::BitwiseComplement | TokenType::LogicalNot => {
                let operator = self.parse_unary_operator()?;
                let inner_factor = self.parse_factor()?;
                Ok(Expression::UnaryOp(operator, Box::new(inner_factor)))
            }
            // Parentheses
            TokenType::OpenParen => {
                let line = peeked_token.line;
                let column = peeked_token.column;
                self.advance();
                let inner_expr = self.parse_expression(0)?; // Minimum precedence is 0 because the expression is parenthesized
                self.consume(TokenType::CloseParen, &format!("Expected closing parenthesis to match opened parenthesis at position {}:{}", line, column))?;

                // Check for postfix increment/decrement on parenthesized expressions
                match self.current_token().token_type {
                    TokenType::Increment => {
                        self.advance();
                        Ok(Expression::PostfixIncrement(Box::new(inner_expr)))
                    }
                    TokenType::Decrement => {
                        self.advance();
                        Ok(Expression::PostfixDecrement(Box::new(inner_expr)))
                    }
                    _ => Ok(inner_expr),
                }
            }
            TokenType::Identifier(ref name) => {
                let var_name = name.clone();
                self.advance();
                // Check for postfix increment/decrement
                match self.current_token().token_type {
                    TokenType::Increment => {
                        self.advance();
                        Ok(Expression::PostfixIncrement(Box::new(
                            Expression::Variable(var_name),
                        )))
                    }
                    TokenType::Decrement => {
                        self.advance();
                        Ok(Expression::PostfixDecrement(Box::new(
                            Expression::Variable(var_name),
                        )))
                    }
                    _ => Ok(Expression::Variable(var_name)),
                }
            }
            _ => {
                let error = ParseError {
                    line: peeked_token.line,
                    column: peeked_token.column,
                    message: format!(
                        "Expected a factor expression (integer literal, unary operator, variable or parenthesized expression), found {:?}",
                        peeked_token.token_type
                    ),
                };
                Err(error)
            }
        }
    }
    // To parse the expression precedence climbing is used to handle precedence and associativity
    fn parse_expression(&mut self, minimum_precedence: u8) -> Result<Expression, ParseError> {
        let mut left_expr = self.parse_factor()?;
        while self.current_token().token_type.is_binary_operator()
            || self.current_token().token_type == TokenType::Assignment
            || self.is_compound_assignment(&self.current_token().token_type)
        {
            let precedence = self
                .current_token()
                .token_type
                .get_precedence()
                .expect("Expected a binary operator or assignment here");

            if precedence < minimum_precedence {
                break;
            }

            match self.current_token().token_type {
                TokenType::Assignment => {
                    // Assignment operator is right associative
                    self.advance(); // Consume = as it was already checked
                    let right_expression = self.parse_expression(precedence)?; // Do not increase precedence for right associative operators
                    left_expr = Expression::Assignment {
                        lvalue: Box::new(left_expr),
                        value: Box::new(right_expression),
                    }
                }
                _ if self.is_compound_assignment(&self.current_token().token_type) => {
                    // Compound assignment operators are right associative
                    let operator = self.parse_compound_assignment_operator()?;
                    let right_expression = self.parse_expression(precedence)?;
                    left_expr = Expression::CompoundAssignment {
                        operator,
                        lvalue: Box::new(left_expr),
                        value: Box::new(right_expression),
                    }
                }
                _ => {
                    // All other operators are left associative
                    let operator = self.parse_binary_operator()?;
                    let right_expression = self.parse_expression(precedence + 1)?; // Increase for left associative
                    left_expr = Expression::BinaryOperator {
                        operator,
                        left: Box::new(left_expr),
                        right: Box::new(right_expression),
                    }
                }
            };
        }

        Ok(left_expr)
    }

    fn parse_unary_operator(&mut self) -> Result<UnaryOperator, ParseError> {
        let operator = match self.current_token().token_type {
            TokenType::Negation => UnaryOperator::Negation,
            TokenType::BitwiseComplement => UnaryOperator::BitwiseComplement,
            TokenType::LogicalNot => UnaryOperator::LogicalNot,
            _ => {
                let current = self.current_token();
                let error = ParseError {
                    line: current.line,
                    column: current.column,
                    message: format!(
                        "Expected a unary operator (- or ~), found {:?}",
                        current.token_type
                    ),
                };
                return Err(error);
            }
        };
        self.advance();
        Ok(operator)
    }

    fn parse_binary_operator(&mut self) -> Result<BinaryOperator, ParseError> {
        let operator = match self.current_token().token_type {
            TokenType::Add => BinaryOperator::Addition,
            TokenType::Negation => BinaryOperator::Subtraction, // Note: Negation here is used as subtraction in binary context
            TokenType::Multiply => BinaryOperator::Multiplication,
            TokenType::Divide => BinaryOperator::Division,
            TokenType::Modulus => BinaryOperator::Modulus,
            TokenType::BitwiseAnd => BinaryOperator::BitwiseAnd,
            TokenType::BitwiseOr => BinaryOperator::BitwiseOr,
            TokenType::BitwiseXor => BinaryOperator::BitwiseXor,
            TokenType::ShiftLeft => BinaryOperator::ShiftLeft,
            TokenType::ShiftRight => BinaryOperator::ShiftRight,
            TokenType::LogicalAnd => BinaryOperator::LogicalAnd,
            TokenType::LogicalOr => BinaryOperator::LogicalOr,
            TokenType::Equal => BinaryOperator::Equal,
            TokenType::NotEqual => BinaryOperator::NotEqual,
            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            TokenType::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            _ => {
                let current = self.current_token();
                let error = ParseError {
                    line: current.line,
                    column: current.column,
                    message: format!(
                        "Expected a binary operator (+, -, *, /, % or bitwise operator), found {:?}",
                        current.token_type
                    ),
                };
                return Err(error);
            }
        };
        self.advance();
        Ok(operator)
    }

    fn is_compound_assignment(&self, token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::AddAssignment
                | TokenType::SubtractAssignment
                | TokenType::MultiplyAssignment
                | TokenType::DivideAssignment
                | TokenType::ModulusAssignment
                | TokenType::BitwiseAndAssignment
                | TokenType::BitwiseOrAssignment
                | TokenType::BitwiseXorAssignment
                | TokenType::ShiftLeftAssignment
                | TokenType::ShiftRightAssignment
        )
    }

    fn parse_compound_assignment_operator(
        &mut self,
    ) -> Result<CompoundAssignmentOperator, ParseError> {
        let operator = match self.current_token().token_type {
            TokenType::AddAssignment => CompoundAssignmentOperator::AddAssignment,
            TokenType::SubtractAssignment => CompoundAssignmentOperator::SubtractAssignment,
            TokenType::MultiplyAssignment => CompoundAssignmentOperator::MultiplyAssignment,
            TokenType::DivideAssignment => CompoundAssignmentOperator::DivideAssignment,
            TokenType::ModulusAssignment => CompoundAssignmentOperator::ModulusAssignment,
            TokenType::BitwiseAndAssignment => CompoundAssignmentOperator::BitwiseAndAssignment,
            TokenType::BitwiseOrAssignment => CompoundAssignmentOperator::BitwiseOrAssignment,
            TokenType::BitwiseXorAssignment => CompoundAssignmentOperator::BitwiseXorAssignment,
            TokenType::ShiftLeftAssignment => CompoundAssignmentOperator::ShiftLeftAssignment,
            TokenType::ShiftRightAssignment => CompoundAssignmentOperator::ShiftRightAssignment,
            _ => {
                let current = self.current_token();
                let error = ParseError {
                    line: current.line,
                    column: current.column,
                    message: format!(
                        "Expected a compound assignment operator (+=, -=, *=, etc.), found {:?}",
                        current.token_type
                    ),
                };
                return Err(error);
            }
        };
        self.advance();
        Ok(operator)
    }

    fn consume(&mut self, expected: TokenType, message: &str) -> Result<TokenType, ParseError> {
        // Compare only the variant (ignore inner values)
        if discriminant(&self.current_token().token_type) != discriminant(&expected) {
            return Err(ParseError {
                line: self.current_token().line,
                column: self.current_token().column,
                message: message.to_string(),
            });
        }

        let consumed_token = Ok(self.current_token().token_type.clone());
        self.advance();
        consumed_token
    }

    fn advance(&mut self) {
        self.current_index += 1;
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current_index]
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<ProgramAst, Vec<ParseError>> {
    Parser::new(&tokens).parse()
}
