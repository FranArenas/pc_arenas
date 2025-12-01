use crate::backend::intermediate_representation::ir_definition::{
    BinaryOperatorIR, FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
};
use crate::frontend::program_ast::{
    BinaryOperator, Expression, Factor, FunctionDefinition, ProgramAst, Statement, UnaryOperator,
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

pub fn generate_ir(ast: ProgramAst) -> ProgramIR {
    match ast {
        ProgramAst::Program(func_def) => {
            ProgramIR::ProgramIR(process_function_definition_ir(func_def))
        }
    }
}

fn process_function_definition_ir(func: FunctionDefinition) -> FunctionDefinitionIR {
    match func {
        FunctionDefinition::Function { identifier, body } => match body {
            Statement::Return(expr) => {
                let instructions = process_expression_ir(expr);
                FunctionDefinitionIR::Function {
                    identifier: identifier.clone(),
                    body: instructions,
                }
            }
        },
    }
}
fn process_expression_ir(expression: Expression) -> Vec<InstructionIR> {
    // generates all the instructions
    let mut instructions = Vec::new();
    let return_value = process_expression_ir_recursive(expression, &mut instructions); // The ValueIR that holds the result of the expression in a value (either a tmp variable or a constant)
    instructions.push(InstructionIR::ReturnIR {
        value: return_value,
    }); // todo: Move to the block were the function definition is created if needed
    instructions
}

// Returns the ValueIR that holds the result of the expression stored in a value (either a tmp variable or a constant)
fn process_expression_ir_recursive(
    expression: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    match expression {
        Expression::Factor(factor) => process_factor_ir(factor, instructions),
        Expression::BinaryOperator {
            operator,
            left,
            right,
        } => {
            let left_expr_val = process_expression_ir_recursive(*left, instructions);
            let right_expr_val = process_expression_ir_recursive(*right, instructions);
            let tmp_var_name = generate_tmp_var_name();
            let operator_ir = match operator {
                BinaryOperator::Addition => BinaryOperatorIR::AdditionIR,
                BinaryOperator::Subtraction => BinaryOperatorIR::SubtractionIR,
                BinaryOperator::Multiplication => BinaryOperatorIR::MultiplicationIR,
                BinaryOperator::Division => BinaryOperatorIR::DivisionIR,
                BinaryOperator::Modulus => BinaryOperatorIR::ModulusIR,
                BinaryOperator::BitwiseAnd => BinaryOperatorIR::BitwiseAndIR,
                BinaryOperator::BitwiseOr => BinaryOperatorIR::BitwiseOrIR,
                BinaryOperator::BitwiseXor => BinaryOperatorIR::BitwiseXorIR,
                BinaryOperator::ShiftLeft => BinaryOperatorIR::ShiftLeftIR,
                BinaryOperator::ShiftRight => BinaryOperatorIR::ShiftRightIR,
            };
            let dst_value = ValueIR::VariableIR(tmp_var_name);
            instructions.push(InstructionIR::BinaryIR {
                binary_operator: operator_ir,
                left_operand: left_expr_val,
                right_operand: right_expr_val,
                dst: dst_value.clone(),
            });
            dst_value
        }
    }
}

// Returns the ValueIR that holds the result of the expression stored in a value (either a tmp variable or a constant)
fn process_factor_ir(factor: Factor, instructions: &mut Vec<InstructionIR>) -> ValueIR {
    match factor {
        Factor::IntLiteral(value) => ValueIR::ConstantIR(value),
        Factor::UnaryOp(operator, expr) => {
            let operand_val =
                process_expression_ir_recursive(Expression::Factor(*expr), instructions);
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
        Factor::Expression(expr) => process_expression_ir_recursive(*expr, instructions),
    }
}
