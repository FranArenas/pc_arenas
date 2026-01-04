use std::fmt::{self, Debug};
use std::mem::discriminant;

use crate::frontend::lexer::{Token, TokenType};
use crate::frontend::program_ast::{
    BinaryOperator, Block, BlockItem, CompoundAssignmentOperator, Declaration, Expression,
    ForInitialization, FunctionDefinition, ProgramAst, Statement, UnaryOperator,
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

        Ok(FunctionDefinition::Function {
            identifier: name,
            body: self.parse_block()?,
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut body = Vec::new();
        self.consume(TokenType::OpenBrace, "Expected opened brace")?;

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
        Ok(Block { items: body })
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
            TokenType::IfKeyword => {
                self.advance(); // Consume 'if'
                self.consume(TokenType::OpenParen, "Expected '(' after 'if'")?;
                let condition = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(TokenType::CloseParen, "Expected ')' after if condition")?;

                let then_branch = Box::new(self.parse_statement()?);

                let else_branch = if self.current_token().token_type == TokenType::ElseKeyword {
                    self.advance(); // Consume 'else'
                    Some(Box::new(self.parse_statement()?))
                } else {
                    None
                };

                return Ok(Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                });
            }
            TokenType::OpenBrace => {
                return Ok(Statement::CompoundStatement(self.parse_block()?));
            }
            TokenType::DoKeyword => {
                self.advance(); // Consume 'do'

                let body = Box::new(self.parse_statement()?);

                self.consume(TokenType::WhileKeyword, "Expected 'while' after 'do' body")?;
                self.consume(TokenType::OpenParen, "Expected '(' after 'while'")?;
                let condition = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(
                    TokenType::CloseParen,
                    "Expected ')' after do-while condition",
                )?;
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after do-while statement",
                )?;

                return Ok(Statement::DoWhile {
                    body,
                    condition,
                    label: None,
                });
            }
            TokenType::WhileKeyword => {
                self.advance(); // Consume 'while'
                self.consume(TokenType::OpenParen, "Expected '(' after 'while'")?;
                let condition = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(TokenType::CloseParen, "Expected ')' after while condition")?;

                let body = Box::new(self.parse_statement()?);

                return Ok(Statement::While {
                    condition,
                    body,
                    label: None,
                });
            }
            TokenType::BreakKeyword => {
                self.advance(); // Consume 'break'
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after break statement",
                )?;
                return Ok(Statement::Break { label: None });
            }
            TokenType::ContinueKeyword => {
                self.advance(); // Consume 'continue'
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after continue statement",
                )?;
                return Ok(Statement::Continue { label: None });
            }
            TokenType::ForKeyword => {
                self.advance(); // Consume 'for'
                self.consume(TokenType::OpenParen, "Expected '(' after 'for'")?;

                let initialization = self.parse_for_initialization()?;

                let condition = if self.current_token().token_type != TokenType::Semicolon {
                    Some(self.parse_expression(0)?) // Minimum precedence is 0
                } else {
                    None
                };
                self.consume(
                    TokenType::Semicolon,
                    "Expected ';' after for loop condition",
                )?;

                let post = if self.current_token().token_type != TokenType::CloseParen {
                    Some(self.parse_expression(0)?) // Minimum precedence is 0
                } else {
                    None
                };
                self.consume(
                    TokenType::CloseParen,
                    "Expected ')' after for loop post expression",
                )?;

                let body = Box::new(self.parse_statement()?);

                return Ok(Statement::For {
                    initialization,
                    condition,
                    post,
                    body,
                    label: None,
                });
            }
            _ => {
                // Parse expressions statement. Only used for expressions with side effects
                let expr = self.parse_expression(0)?; // Minimum precedence is 0
                self.consume(
                    TokenType::Semicolon,
                    &format!(
                        "Expected semicolon after expression statement. Got: {:?}",
                        self.current_token().token_type
                    ),
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
        let mut expr = match peeked_token.token_type {
            TokenType::IntLiteral(value) => {
                self.advance();
                Expression::Constant(value)
            }
            // Prefix increment/decrement
            TokenType::Increment => {
                self.advance();
                let inner_expr = self.parse_factor()?;
                Expression::PrefixIncrement(Box::new(inner_expr))
            }
            TokenType::Decrement => {
                self.advance();
                let inner_expr = self.parse_factor()?;
                Expression::PrefixDecrement(Box::new(inner_expr))
            }
            // Unary operators
            TokenType::Negation | TokenType::BitwiseComplement | TokenType::LogicalNot => {
                let operator = self.parse_unary_operator()?;
                let inner_factor = self.parse_factor()?;
                Expression::UnaryOp(operator, Box::new(inner_factor))
            }
            // Parentheses
            TokenType::OpenParen => {
                let line = peeked_token.line;
                let column = peeked_token.column;
                self.advance();
                let inner_expr = self.parse_expression(0)?; // Minimum precedence is 0 because the expression is parenthesized
                self.consume(TokenType::CloseParen, &format!("Expected closing parenthesis to match opened parenthesis at position {}:{}", line, column))?;
                inner_expr
            }
            TokenType::Identifier(ref name) => {
                let var_name = name.clone();
                self.advance();
                Expression::Variable(var_name)
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
                return Err(error);
            }
        };

        // Postfix Operators are special as they can be chained
        // In the case of ++-- is invalid semantically but not syntactically. It will be handled in semantic analysis as invalid.
        loop {
            match self.current_token().token_type {
                TokenType::Increment => {
                    self.advance();
                    expr = Expression::PostfixIncrement(Box::new(expr));
                }
                TokenType::Decrement => {
                    self.advance();
                    expr = Expression::PostfixDecrement(Box::new(expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }
    // To parse the expression precedence climbing is used to handle precedence and associativity
    fn parse_expression(&mut self, minimum_precedence: u8) -> Result<Expression, ParseError> {
        let mut left_expr = self.parse_factor()?;
        while self.current_token().token_type.is_binary_operator()
            || self.current_token().token_type == TokenType::Assignment
            || self.current_token().token_type.is_compound_assignment()
            || self.current_token().token_type == TokenType::QuestionMark
        // Ternary conditional operator
        {
            let precedence = self
                .current_token()
                .token_type
                .get_precedence()
                .expect("Expected a binary operator, assignment, compound assignment or ternary conditional operator here");

            if precedence < minimum_precedence {
                break;
            }

            if self.current_token().token_type.is_compound_assignment() {
                // Compound assignment operators are right associative
                let operator = self.parse_compound_assignment_operator()?;
                let right_expression = self.parse_expression(precedence)?;
                left_expr = Expression::CompoundAssignment {
                    operator,
                    lvalue: Box::new(left_expr),
                    value: Box::new(right_expression),
                }
            } else {
                match self.current_token().token_type {
                    TokenType::Assignment => {
                        // Assignment operator is right associative and a special case
                        self.advance(); // Consume = as it was already checked
                        let right_expression = self.parse_expression(precedence)?; // Do not increase precedence for right associative operators
                        left_expr = Expression::Assignment {
                            lvalue: Box::new(left_expr),
                            value: Box::new(right_expression),
                        }
                    }
                    TokenType::QuestionMark => {
                        // Ternary conditional operator is right associative and a special case
                        // We treat it as a sub-expression with its own parsing, the left expression is parsed with 0 and then the else expression is parsed
                        self.advance(); // Consume ?
                        let then_expression = self.parse_expression(0)?; // Minimum precedence for then expression
                        self.consume(
                            TokenType::Colon,
                            "Expected ':' in conditional (ternary) operator",
                        )?;
                        let right_else_expression = self.parse_expression(precedence)?; // Do not increase precedence for right associative operators
                        left_expr = Expression::Conditional {
                            condition: Box::new(left_expr),
                            then_expr: Box::new(then_expression),
                            else_expr: Box::new(right_else_expression),
                        }
                    }
                    _ => {
                        // All other operators are left associative (binary operators)
                        let operator = self.parse_binary_operator()?;
                        let right_expression = self.parse_expression(precedence + 1)?; // Increase for left associative
                        left_expr = Expression::BinaryOperator {
                            operator,
                            left: Box::new(left_expr),
                            right: Box::new(right_expression),
                        }
                    }
                }
            };
        }

        Ok(left_expr)
    }

    fn parse_for_initialization(&mut self) -> Result<Option<ForInitialization>, ParseError> {
        let value;
        match self.current_token().token_type {
            TokenType::Semicolon => {
                // No initialization
                value = Ok(None)
            }
            TokenType::IntKeyword => {
                // Declaration initialization
                let declaration = self.parse_declaration()?; // The semicolon is consumed inside parse_declaration
                return Ok(Some(ForInitialization::InitDeclaration(declaration)));
            }
            _ => {
                // Expression initialization
                let expr = Some(self.parse_expression(0)?); // Minimum precedence is 0
                value = Ok(Some(ForInitialization::InitExpression(expr)))
            }
        }
        self.consume(
            TokenType::Semicolon,
            &format!(
                "Expected ';' after for loop initialization. Got: {:?}",
                self.current_token().token_type
            ),
        )?;
        value
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
