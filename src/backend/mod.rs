pub mod code_emission;
pub mod intermediate_representation;

pub use code_emission::{
    CodeEmissionError, CodeEmitter, CodeGenError, FunctionDefinitionAssembly, InstructionAssembly,
    OperandAssembly, ProgramAssembly, assembly_definition, assembly_generation, generate_code,
};
