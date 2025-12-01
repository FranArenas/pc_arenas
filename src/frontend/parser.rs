use std::fmt::{self, Debug};
use std::mem::discriminant;

use crate::frontend::lexer::{Token, TokenType};
use crate::frontend::program_ast::{
    BinaryOperator, Expression, Factor, FunctionDefinition, ProgramAst, Statement, UnaryOperator,
};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error at line {}, column {}: {}", self.line, self.column, self.message)
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

        let body = self.parse_statement()?;

        self.consume(TokenType::CloseBrace, "Expected closed brace")?;

        Ok(FunctionDefinition::Function {
            identifier: name.clone(),
            body: body.clone(),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::ReturnKeyword, "Expected return keyword")?;
        let return_value = self.parse_expression(0)?;
        self.consume(
            TokenType::Semicolon,
            "Expected semicolon after return statement",
        )?;
        Ok(Statement::Return(return_value))
    }

    fn parse_factor(&mut self) -> Result<Factor, ParseError> {
        let peeked_token = self.current_token();
        match peeked_token.token_type {
            TokenType::IntLiteral(value) => {
                self.advance();
                Ok(Factor::IntLiteral(value))
            }
            // Unary operators
            TokenType::Negation | TokenType::BitwiseComplement => {
                let operator = self.parse_unary_operator()?;
                let inner_factor = self.parse_factor()?;
                Ok(Factor::UnaryOp(operator, Box::new(inner_factor)))
            }
            // Parentheses
            TokenType::OpenParen => {
                let line = peeked_token.line;
                let column = peeked_token.column;
                self.advance();
                let inner_expr = self.parse_expression(0)?; // Minimum precedence is 0 because the expression is parenthesized
                self.consume(TokenType::CloseParen, &format!("Expected closing parenthesis to match opened parenthesis at position {}:{}", line, column))?;
                Ok(Factor::Expression(Box::new(inner_expr)))
            }
            _ => {
                let error = ParseError {
                    line: peeked_token.line,
                    column: peeked_token.column,
                    message: format!(
                        "Expected a factor (integer literal, unary operator, or parenthesized expression), found {:?}",
                        peeked_token.token_type
                    ),
                };
                Err(error)
            }
        }
    }
    // To parse the expression precedence climbing is used to handle precedence and associativity
    fn parse_expression(&mut self, minimum_precedence: u8) -> Result<Expression, ParseError> {
        let mut left_expr = Expression::Factor(self.parse_factor()?); //todo: Fix this. It is broken
        while self.current_token().token_type.is_binary_operator() {
            let precedence = self
                .current_token()
                .token_type
                .get_precedence()
                .expect("Expected a binary operator here");
            if precedence < minimum_precedence {
                break;
            }
            let operator = self.parse_binary_operator()?;
            let right_expression = self.parse_expression(precedence + 1)?;
            left_expr = Expression::BinaryOperator {
                operator,
                left: Box::new(left_expr),
                right: Box::new(right_expression),
            };
        }

        Ok(left_expr)
    }

    fn parse_unary_operator(&mut self) -> Result<UnaryOperator, ParseError> {
        let operator = match self.current_token().token_type {
            TokenType::Negation => UnaryOperator::Negation,
            TokenType::BitwiseComplement => UnaryOperator::BitwiseComplement,
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


fn consume(
    &mut self,
    expected: TokenType,
    message: &str,
) -> Result<TokenType, ParseError> {
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
