pub mod code_emission;
pub mod intermediate_representation;

pub use code_emission::{
    assembly_definition, assembly_generation, generate_code, CodeEmissionError, CodeEmitter,
    CodeGenError, FunctionDefinitionAssembly, InstructionAssembly, OperandAssembly,
    ProgramAssembly,
};
