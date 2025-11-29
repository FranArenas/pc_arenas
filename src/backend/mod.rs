pub mod assemblyx86;
pub mod intermediate_representation;

pub use assemblyx86::{
    CodeEmissionError, CodeEmitter, FunctionDefinitionAssembly, InstructionAssembly,
    OperandAssembly, ProgramAssembly, assembly_definition, assembly_generation, generate_code,
};
