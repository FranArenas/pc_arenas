pub mod cli;
pub use cli::Cli;

pub mod lexer;
pub use lexer::{tokenize, LexError, Token, TokenType};

pub mod parser;
pub use parser::{parse, ParseError};

pub mod ast;
pub use ast::{Expression, FunctionDefinition, ProgramAst, Statement};
