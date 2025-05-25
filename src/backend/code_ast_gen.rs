use crate::backend::assembly_ast::{
    FunctionDefinitionAssembly, Instruction, Operand, ProgramAssembly,
};
use crate::frontend::program_ast::{Expression, FunctionDefinition, ProgramAst, Statement};

#[derive(Debug, Clone)]
pub struct CodeGenError {
    pub message: String,
}

pub fn generate_code(ast: &ProgramAst) -> Result<ProgramAssembly, CodeGenError> {
    match ast {
        ProgramAst::Program(func_def) => Ok(ProgramAssembly::ProgramAssembly(
            generate_function_definition(func_def)?,
        )),
    }
}

fn generate_function_definition(
    func: &FunctionDefinition,
) -> Result<FunctionDefinitionAssembly, CodeGenError> {
    match func {
        FunctionDefinition::Function { identifier, body } => {
            let instructions = generate_instructions(body)?;
            Ok(FunctionDefinitionAssembly::Function {
                identifier: identifier.clone(),
                instructions,
            })
        }
    }
}

fn generate_instructions(stmt: &Statement) -> Result<Vec<Instruction>, CodeGenError> {
    match stmt {
        Statement::Return(expr) => {
            let operand = match expr {
                Expression::Constant(value) => generate_operand(*value),
            };
            Ok(vec![
                Instruction::Mov {
                    src: operand,
                    dst: Operand::Register,
                },
                Instruction::Ret,
            ])
        }
    }
}

fn generate_operand(value: i64) -> Operand {
    Operand::Immediate(value)
}
