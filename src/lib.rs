pub mod cli;
pub use cli::Cli;

pub mod utils;

pub mod frontend;
pub use frontend::lexer::{LexError, Token, TokenType, tokenize};
pub use frontend::parser::{ParseError, parse};
pub use frontend::program_ast::{Expression, FunctionDefinition, ProgramAst, Statement};
pub use frontend::semantic::semantic_analysis::run_semantic_analysis;

pub mod backend;
pub use backend::assembly_definition::ProgramAssembly;
pub use backend::assembly_generation::generate_code;
pub use backend::assemblyx86::{CodeEmissionError, CodeEmitter};
