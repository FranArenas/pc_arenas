use crate::backend::intermediate_representation::ir_definition::{
    FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
};
use crate::frontend::program_ast::{
    Expression, FunctionDefinition, ProgramAst, Statement, UnaryOperator,
};
use std::sync::atomic::{AtomicUsize, Ordering};

static TMP_VAR_COUNT: AtomicUsize = AtomicUsize::new(0);

fn generate_tmp_var_name() -> String {
    let id = TMP_VAR_COUNT.fetch_add(1, Ordering::Relaxed) + 1; // increment atomically
    format!("tmp.{}", id) // Add period to avoid conflicts with user-defined variables. User-defined variables cannot contain periods.
}

#[derive(Debug, Clone)]
pub struct IrGenError {
    pub message: String,
}

pub fn generate_ir(ast: &ProgramAst) -> ProgramIR {
    match ast {
        ProgramAst::Program(func_def) => ProgramIR::ProgramIR(process_function_definition_ir(
            func_def,
        )),
    }
}

fn process_function_definition_ir(
    func: &FunctionDefinition,
) -> FunctionDefinitionIR {
    match func {
        FunctionDefinition::Function { identifier, body } => match body {
            Statement::Return(expr) => {
                let instructions = process_instructions_ir(expr);
                FunctionDefinitionIR::Function {
                    identifier: identifier.clone(),
                    body: instructions,
                }
            }
        },
    }
}
fn process_instructions_ir(expression: &Expression) -> Vec<InstructionIR> {
    // generates all the instructions
    let mut instructions = Vec::new();
    let value = process_instructions_ir_recursive(expression, &mut instructions);
    instructions.push(InstructionIR::ReturnIR { value }); // todo: Move to the block were the function definition is created if needed
    instructions
}

fn process_instructions_ir_recursive(
    expression: &Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    match expression { 
        Expression::Constant(value) => {
            let constant_value = ValueIR::ConstantIR(*value);
            constant_value
        }
        Expression::UnaryOp(operator, expr) => {
            let operand_val = process_instructions_ir_recursive(expr, instructions);
            let tmp_var_name = generate_tmp_var_name();
            let operator_ir = match operator {
                UnaryOperator::Negation => UnaryOperatorIR::NegationIR,
                UnaryOperator::BitwiseComplement => UnaryOperatorIR::BitwiseComplementIR,
            };
            let dst_value = ValueIR::VariableIR(tmp_var_name);
            instructions.push(InstructionIR::UnaryIR {
                unary_operator: operator_ir,
                src: operand_val,
                dst: dst_value.clone(),
            });
            dst_value
        }
    }
}
