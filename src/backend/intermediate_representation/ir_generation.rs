use crate::backend::intermediate_representation::ir_definition::{
    BinaryOperatorIR, FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
};
use crate::frontend::program_ast::{
    BinaryOperator, BlockItem, Declaration, Expression, FunctionDefinition, ProgramAst, Statement,
    UnaryOperator,
};
use crate::utils::tmp_var_counter::TMP_VAR_COUNT;
use std::sync::atomic::{AtomicUsize, Ordering};

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
        FunctionDefinition::Function { identifier, body } => {
            let mut instructions = Vec::new(); // Vector that will be filled with IR instructions
            for item in body {
                match item {
                    BlockItem::Statement(stmt) => {
                        process_statement_ir(stmt, &mut instructions);
                    }
                    BlockItem::Declaration(decl) => {
                        process_declaration_ir(decl, &mut instructions);
                    }
                }
            }
            // Ensure function ends with a return instruction. This return will be eliminated later if there is already a return instruction at the end of the function. This is done because the C standard doesn't define the behavior of reaching the end of a non-void function without a return statement. This is undefined behavior, and adding the return 0 simplifies the compiler implementation. Reference: Section 5.1.2.2.3 and Section 6.9.1, paragraph 12
            instructions.push(InstructionIR::ReturnIR {
                value: ValueIR::ConstantIR(0),
            });

            FunctionDefinitionIR::Function {
                identifier: identifier.clone(),
                body: instructions,
            }
        }
    }
}

fn process_statement_ir(stmt: Statement, instructions: &mut Vec<InstructionIR>) -> Option<ValueIR> {
    match stmt {
        Statement::Return(expr) => {
            let return_value = process_expression_ir(expr, instructions);
            instructions.push(InstructionIR::ReturnIR {
                value: return_value.clone(),
            });
            Some(return_value)
        }
        Statement::Expression(expr) => Some(process_expression_ir(expr, instructions)),
        Statement::Null => None,
    }
}

fn process_declaration_ir(
    decl: Declaration,
    instructions: &mut Vec<InstructionIR>,
) -> Option<ValueIR> {
    match decl {
        Declaration::VariableDeclaration {
            identifier,
            initial_value,
        } => {
            if let Some(init_expr) = initial_value {
                Some(process_variable_assignment_ir(
                    identifier,
                    init_expr,
                    instructions,
                ))
            } else {
                // No explicit instruction needed for variable declaration without initialization. It is just ignored in IR.
                None
            }
        }
    }
}

// Returns the ValueIR that holds the result of the expression stored in a value (either a tmp variable or a constant)
fn process_expression_ir(expression: Expression, instructions: &mut Vec<InstructionIR>) -> ValueIR {
    match expression {
        Expression::Constant(value) => ValueIR::ConstantIR(value),
        Expression::Variable(identifier) => ValueIR::VariableIR(identifier.clone()),
        Expression::UnaryOp(operator, expr) => {
            process_unary_operator_ir(operator, *expr, instructions)
        }
        Expression::BinaryOperator {
            operator,
            left,
            right,
        } => process_binary_operator_ir(operator, *left, *right, instructions),
        Expression::Assignment { lvalue, value } => {
            process_assignment_ir(*lvalue, *value, instructions)
        }
    }
}

fn process_unary_operator_ir(
    operator: UnaryOperator,
    expr: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    let operand_val = process_expression_ir(expr, instructions);
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

fn process_binary_operator_ir(
    operator: BinaryOperator,
    left: Expression,
    right: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    // Handle short-circuit operators
    match operator {
        BinaryOperator::LogicalAnd => {
            return process_logical_and_ir(left, right, instructions);
        }
        BinaryOperator::LogicalOr => {
            return process_logical_or_ir(left, right, instructions);
        }
        _ => {}
    }

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
        BinaryOperator::Equal => BinaryOperatorIR::EqualIR,
        BinaryOperator::NotEqual => BinaryOperatorIR::NotEqualIR,
        BinaryOperator::LessThan => BinaryOperatorIR::LessThanIR,
        BinaryOperator::LessThanOrEqual => BinaryOperatorIR::LessThanOrEqualIR,
        BinaryOperator::GreaterThan => BinaryOperatorIR::GreaterThanIR,
        BinaryOperator::GreaterThanOrEqual => BinaryOperatorIR::GreaterThanOrEqualIR,
        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
            unreachable!(
                "Logical operators should have been handled above. This is a compiler bug."
            )
        }
    };
    let left_expr_val = process_expression_ir(left, instructions);
    let right_expr_val = process_expression_ir(right, instructions);
    let dst_value = ValueIR::VariableIR(tmp_var_name);
    instructions.push(InstructionIR::BinaryIR {
        binary_operator: operator_ir,
        left_operand: left_expr_val,
        right_operand: right_expr_val,
        dst: dst_value.clone(),
    });
    dst_value
}

fn process_assignment_ir(
    lvalue: Expression,
    value: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    if let Expression::Variable(identifier) = lvalue {
        process_variable_assignment_ir(identifier, value, instructions)
    } else {
        unreachable!(
            "Left-hand side of assignment must be a variable. It should have been caught during semantic analysis. This is a compiler bug."
        )
    }
}

fn process_variable_assignment_ir(
    identifier: String,
    value_expression: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    let value = process_expression_ir(value_expression, instructions);
    let dst_value = ValueIR::VariableIR(identifier); // The variable ID is already unique as we use the same static counter for tacky and semantic analysis variable resolution
    instructions.push(InstructionIR::CopyIR {
        src: value,
        dst: dst_value.clone(),
    });
    dst_value
}

fn process_logical_and_ir(
    left_expression: Expression,
    right_expression: Expression,
    instructions: &mut Vec<InstructionIR>,
) -> ValueIR {
    let short_circuit_label = add_suffix_to_label_name("logical_and_short_circuit");
    let end_label = add_suffix_to_label_name("logical_and_end");
    let result_value = ValueIR::VariableIR(generate_tmp_var_name(Some("logical_and_result")));

    let left_value = process_expression_ir(left_expression, instructions);
    instructions.push(InstructionIR::JumpIfZeroIR {
        condition: left_value,
        label_identifier: short_circuit_label.clone(),
    });

    let right_value = process_expression_ir(right_expression, instructions);
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

    let left_value = process_expression_ir(left_expression, instructions);
    instructions.push(InstructionIR::JumpIfNotZeroIR {
        condition: left_value,
        label_identifier: short_circuit_label.clone(),
    });

    let right_value = process_expression_ir(right_expression, instructions);
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
