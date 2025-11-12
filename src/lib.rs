pub mod cli;
pub use cli::Cli;

pub mod frontend;
pub use frontend::lexer::{tokenize, LexError, Token, TokenType};
pub use frontend::parser::{parse, ParseError};
pub use frontend::program_ast::{Expression, FunctionDefinition, ProgramAst, Statement};

pub mod backend;
pub use backend::assembly_definition::ProgramAssembly;
pub use backend::assembly_generation::{generate_code, CodeGenError};
pub use backend::code_emission::{CodeEmissionError, CodeEmitter};
