pub mod cli;
pub use cli::Cli;

pub mod frontend;
pub use frontend::lexer::{LexError, Token, TokenType, tokenize};
pub use frontend::parser::{ParseError, parse};
pub use frontend::program_ast::{Expression, FunctionDefinition, ProgramAst, Statement};

pub mod backend;
pub use backend::assembly_definition::ProgramAssembly;
pub use backend::assembly_generation::{CodeGenError, generate_code};
pub use backend::code_emission::{CodeEmissionError, CodeEmitter};
