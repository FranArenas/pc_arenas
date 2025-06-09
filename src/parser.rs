// todo: In this state the synchronization is not implemented, so we will stop parsing on the first error.
// In the future, we will implement a way to recover from errors and continue parsing using all the methods like peek, advance, and consume.
use std::fmt;

use crate::ast::{Expression, FunctionDefinition, ProgramAst, Statement};
use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: Token,
        position: usize,
        message: String,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                position,
                message,
            } => {
                write!(
                    f,
                    "Unexpected token error at position {}: expected {:?}, found {:?}. Message Error: {}",
                    position, expected, found.token_type, message // todo: Improve formatting for found token printing the expected value
                )
            }
        }
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Result<ProgramAst, ParseError> {
        let function_definition = self.parse_function_definition();
        if function_definition.is_err() {
            return Err(function_definition.err().unwrap());
        }
        let function_definition = function_definition.unwrap();

        self.consume(TokenType::EndOfFile, "Expected end of file")?;

        Ok(ProgramAst::Program(function_definition))
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
        let return_value = self.parse_expression()?;
        self.consume(
            TokenType::Semicolon,
            "Expected semicolon after return statement",
        )?;
        Ok(Statement::Return(return_value))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token()?;

        let expression = match token.token_type {
            TokenType::IntLiteral(value) => Ok(Expression::Constant(value)),
            _ => {
                Err(ParseError::UnexpectedToken {
                    expected: TokenType::IntLiteral(0), // 0 acts as a placeholder
                    found: token.clone(),
                    position: self.current,
                    message: "Unexpected token".to_string(),
                })
            }
        };

        self.advance();

        expression
    }

    fn consume(&mut self, expected: TokenType, message: &str) -> Result<TokenType, ParseError> {
        match self.check(&expected) {
            Ok(_) => { /* all good, continue */ }

            Err(err) => match err {
                ParseError::UnexpectedToken {
                    expected,
                    found,
                    position,
                    ..
                } => {
                    return Err(ParseError::UnexpectedToken {
                        expected,
                        found,
                        position,
                        message: message.to_string(),
                    });
                }
            },
        }

        let consumed_token = Ok(self.current_token()?.token_type.clone());

        self.advance();

        consumed_token
    }

    fn check(&self, expected: &TokenType) -> Result<(), ParseError> {
        let is_expected = match expected {
            TokenType::Identifier(_) => {
                matches!(self.current_token()?.token_type, TokenType::Identifier(_))
            }
            TokenType::IntLiteral(_) => {
                matches!(self.current_token()?.token_type, TokenType::IntLiteral(_))
            }
            _ => &self.current_token()?.token_type == expected,
        };

        if !is_expected {
            return Err(ParseError::UnexpectedToken {
                expected: expected.clone(),
                found: self.current_token()?.clone(),
                position: self.current,
                message: "Unexpected token".to_string(),
            });
        }
        Ok(())
    }

    fn peek(&self) -> &Token {
        if self.current + 1 < self.tokens.len() {
            return &self.tokens[self.current + 1];
        }
        panic!("Tried to peek beyond the end of the token list")
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn current_token(&self) -> Result<&Token, ParseError> {
        Ok(&self.tokens[self.current])
    }

    fn is_end(&self) -> bool {
        self.tokens[self.current].token_type == TokenType::EndOfFile
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<ProgramAst, ParseError> {
    Parser::new(&tokens).parse()
}
