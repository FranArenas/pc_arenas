pub mod cli;
pub use cli::Cli;

pub mod frontend;
pub use frontend::lexer::{tokenize, LexError, Token, TokenType};
pub use frontend::parser::{parse, ParseError};
pub use frontend::program_ast::{Expression, FunctionDefinition, ProgramAst, Statement};

pub mod backend;
pub use backend::assembly_ast::ProgramAssembly;
pub use backend::code_ast_gen::{generate_code, CodeGenError};
pub use backend::code_emission::{CodeEmissionError, CodeEmitter};
