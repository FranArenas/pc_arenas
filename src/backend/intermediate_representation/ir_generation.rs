use crate::backend::intermediate_representation::ir_definition::{
    BinaryOperatorIR, FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
};
use crate::frontend::program_ast::{
    BinaryOperator, Expression, Factor, FunctionDefinition, ProgramAst, Statement, UnaryOperator,
};
use std::sync::atomic::{AtomicUsize, Ordering};

static TMP_VAR_COUNT: AtomicUsize = AtomicUsize::new(0);
static LABEL_MANGLE_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Generates a unique temporary variable name
fn generate_tmp_var_name(prefix: Option<&str>) -> String {
    let id = TMP_VAR_COUNT.fetch_add(1, Ordering::Relaxed);
    match prefix {
        Some(p) => format!("tmp_{}.{}", p, id),
        None => format!("tmp.{}", id),
    }
}

/// Adds a unique suffix to the given label name to avoid conflicts with other labels
fn add_suffix_to_label_name(label: &str) -> String {
    let id = LABEL_MANGLE_COUNT.fetch_add(1, Ordering::Relaxed); // increment atomically
    format!("{}.{}", label, id) // Add period + number to avoid conflicts with labels
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
            let tmp_var_name = generate_tmp_var_name(None);
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
                BinaryOperator::LogicalAnd => {
                    return process_logical_and_ir(*left, *right, instructions);
                }
                BinaryOperator::LogicalOr => {
                    return process_logical_or_ir(*left, *right, instructions);
                }
                BinaryOperator::Equal => BinaryOperatorIR::EqualIR,
                BinaryOperator::NotEqual => BinaryOperatorIR::NotEqualIR,
                BinaryOperator::LessThan => BinaryOperatorIR::LessThanIR,
                BinaryOperator::LessThanOrEqual => BinaryOperatorIR::LessThanOrEqualIR,
                BinaryOperator::GreaterThan => BinaryOperatorIR::GreaterThanIR,
                BinaryOperator::GreaterThanOrEqual => BinaryOperatorIR::GreaterThanOrEqualIR,
            };
            let left_expr_val = process_expression_ir_recursive(*left, instructions);
            let right_expr_val = process_expression_ir_recursive(*right, instructions);
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
            let tmp_var_name = generate_tmp_var_name(None);
            let operator_ir = match operator {
                UnaryOperator::Negation => UnaryOperatorIR::NegationIR,
                UnaryOperator::BitwiseComplement => UnaryOperatorIR::BitwiseComplementIR,
                UnaryOperator::LogicalNot => UnaryOperatorIR::LogicalNotIR,
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

fn process_logical_and_ir(
    left_expression: Expression,
    right_expression: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    let short_circuit_label = add_suffix_to_label_name("logical_and_short_circuit");
    let end_label = add_suffix_to_label_name("logical_and_end");
    let result_value = ValueIR::VariableIR(generate_tmp_var_name(Some("logical_and_result")));

    let left_value = process_expression_ir_recursive(left_expression, instructions);
    instructions.push(InstructionIR::JumpIfZeroIR {
        condition: left_value,
        label_identifier: short_circuit_label.clone(),
    });

    let right_value = process_expression_ir_recursive(right_expression, instructions);
    instructions.push(InstructionIR::JumpIfZeroIR {
        condition: right_value,
        label_identifier: short_circuit_label.clone(),
    });
    // Both are non-zero (true)
    instructions.push(InstructionIR::CopyIR {
        src: ValueIR::ConstantIR(1),
        dst: result_value.clone(),
    });
    instructions.push(InstructionIR::JumpIR {
        label_identifier: end_label.clone(),
    });
    // Short-circuit to false
    instructions.push(InstructionIR::LabelIR {
        identifier: short_circuit_label,
    });
    instructions.push(InstructionIR::CopyIR {
        src: ValueIR::ConstantIR(0),
        dst: result_value.clone(),
    });
    // End label
    instructions.push(InstructionIR::LabelIR {
        identifier: end_label,
    });
    result_value
}

fn process_logical_or_ir(
    left_expression: Expression,
    right_expression: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    let short_circuit_label = add_suffix_to_label_name("logical_or_short_circuit");
    let end_label = add_suffix_to_label_name("logical_or_end");
    let result_value = ValueIR::VariableIR(generate_tmp_var_name(Some("logical_or_result")));

    let left_value = process_expression_ir_recursive(left_expression, instructions);
    instructions.push(InstructionIR::JumpIfNotZeroIR {
        condition: left_value,
        label_identifier: short_circuit_label.clone(),
    });

    let right_value = process_expression_ir_recursive(right_expression, instructions);
    instructions.push(InstructionIR::JumpIfNotZeroIR {
        condition: right_value,
        label_identifier: short_circuit_label.clone(),
    });
    // Both are zero (false)
    instructions.push(InstructionIR::CopyIR {
        src: ValueIR::ConstantIR(0),
        dst: result_value.clone(),
    });
    instructions.push(InstructionIR::JumpIR {
        label_identifier: end_label.clone(),
    });
    // Short-circuit to true
    instructions.push(InstructionIR::LabelIR {
        identifier: short_circuit_label,
    });
    instructions.push(InstructionIR::CopyIR {
        src: ValueIR::ConstantIR(1),
        dst: result_value.clone(),
    });
    // End label
    instructions.push(InstructionIR::LabelIR {
        identifier: end_label,
    });
    result_value
}
