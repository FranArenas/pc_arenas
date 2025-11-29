pub mod assembly_definition;
pub mod assembly_generation;
pub mod code_emission;

pub use assembly_definition::{
    FunctionDefinitionAssembly, InstructionAssembly, OperandAssembly, ProgramAssembly,
};
pub use assembly_generation::generate_code;
pub use code_emission::{CodeEmissionError, CodeEmitter};
