use std::fmt;

use crate::ast::{Expression, FunctionDefinition, ProgramAst, Statement};
use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub expected: TokenType,
    pub found: Token,
    pub position: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Unexpected token error at position {}: expected {:?}, found {:?}. Message Error: {}",
            self.position, self.expected, self.found.token_type, self.message
        )
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
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
        let return_value = self.parse_expression()?;
        self.consume(
            TokenType::Semicolon,
            "Expected semicolon after return statement",
        )?;
        Ok(Statement::Return(return_value))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token();

        let expression = match token.token_type {
            TokenType::IntLiteral(value) => Expression::Constant(value),
            _ => {
                let error = ParseError {
                    expected: TokenType::IntLiteral(0), // Placeholder
                    found: token.clone(),
                    position: self.current,
                    message: "Expected an integer literal".to_string(),
                };
                return Err(error);
            }
        };

        self.advance();

        Ok(expression)
    }

    fn consume(&mut self, expected: TokenType, message: &str) -> Result<TokenType, ParseError> {
        if let Err(mut err) = self.check(&expected) {
            err.message = message.to_string();
            return Err(err);
        }

        let consumed_token = Ok(self.current_token().token_type.clone());

        self.advance();

        consumed_token
    }

    fn check(&mut self, expected: &TokenType) -> Result<(), ParseError> {
        let is_expected = match expected {
            TokenType::Identifier(_) => {
                matches!(self.current_token().token_type, TokenType::Identifier(_))
            }
            TokenType::IntLiteral(_) => {
                matches!(self.current_token().token_type, TokenType::IntLiteral(_))
            }
            _ => &self.current_token().token_type == expected,
        };

        if !is_expected {
            let error = ParseError {
                expected: expected.clone(),
                found: self.current_token().clone(),
                position: self.current,
                message: "Unexpected token".to_string(),
            };
            return Err(error);
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

    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_end(&self) -> bool {
        self.tokens[self.current].token_type == TokenType::EndOfFile
    }

    fn synchronize(&mut self) {
        while !self.is_end() {
            if self.current_token().token_type == TokenType::Semicolon {
                self.advance();
                return;
            }

            match self.peek().token_type {
                TokenType::IntKeyword
                | TokenType::VoidKeyword
                | TokenType::ReturnKeyword
                | TokenType::Identifier(_) => {
                    return;
                }
                _ => self.advance(),
            }
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<ProgramAst, Vec<ParseError>> {
    Parser::new(&tokens).parse()
}
