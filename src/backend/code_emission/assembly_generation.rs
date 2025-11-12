use std::collections::HashMap;

use crate::backend::{
    assembly_definition::{
        FunctionDefinitionAssembly, InstructionAssembly, OperandAssembly, ProgramAssembly,
        RegisterAssembly, UnaryOperatorAssembly,
    },
    intermediate_representation::ir_definition::{
        FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
    },
};

#[derive(Debug, Clone)]
pub struct CodeGenError {
    pub message: String,
}

pub fn generate_code(ast: &ProgramIR) -> Result<ProgramAssembly, CodeGenError> {
    let ast_with_temporal_addresses = match ast {
        ProgramIR::ProgramIR(func_def) => {
            ProgramAssembly::ProgramAssembly(process_function_definition(func_def))
        }
    };

    // Right now there is only one function per program, so we can process it directly. Replace it with a loop later if needed.
    let ProgramAssembly::ProgramAssembly(func_def) = ast_with_temporal_addresses;
    let function_definition_with_stack_addresses = replace_pseudoregisters(func_def)?;

    Ok(ProgramAssembly::ProgramAssembly(
        function_definition_with_stack_addresses,
    ))
}

fn process_function_definition(func: &FunctionDefinitionIR) -> FunctionDefinitionAssembly {
    match func {
        FunctionDefinitionIR::Function { identifier, body } => {
            let instructions = process_instructions(body);
            FunctionDefinitionAssembly::Function {
                identifier: identifier.clone(),
                instructions,
            }
        }
    }
}

fn process_instructions(instructions_ir: &Vec<InstructionIR>) -> Vec<InstructionAssembly> {
    let mut assembly_ins: Vec<InstructionAssembly> = Vec::new();
    for instr_ir in instructions_ir {
        match instr_ir {
            InstructionIR::UnaryIR {
                unary_operator,
                src,
                dst,
            } => assembly_ins.extend(process_unary_instruction(unary_operator, src, dst)),
            InstructionIR::ReturnIR { value } => {
                assembly_ins.extend(process_return_instruction(value))
            }
        }
    }
    assembly_ins
}

fn process_unary_instruction(
    unary_operator: &UnaryOperatorIR,
    src: &ValueIR,
    dst: &ValueIR,
) -> Vec<InstructionAssembly> {
    let mut instructions = Vec::new();
    let operator = match unary_operator {
        UnaryOperatorIR::NegationIR => UnaryOperatorAssembly::NegationAssembly,
        UnaryOperatorIR::BitwiseComplementIR => UnaryOperatorAssembly::BitwiseNotAssembly,
    };
    let src_operand = process_value(src);
    let dst_operand = process_value(dst);

    instructions.push(InstructionAssembly::Mov {
        src: src_operand,
        dst: dst_operand.clone(),
    });
    instructions.push(InstructionAssembly::Unary {
        unary_operator: operator,
        operand: dst_operand,
    });
    instructions
}

fn process_return_instruction(value: &ValueIR) -> Vec<InstructionAssembly> {
    let mut instructions = Vec::new();
    let src_operand = process_value(value);
    instructions.push(InstructionAssembly::Mov {
        src: src_operand,
        dst: OperandAssembly::Register(RegisterAssembly::Ax),
    }); // Placeholder for MOV instruction to move return value to appropriate register
    instructions.push(InstructionAssembly::Ret);
    instructions
}

fn process_value(value: &ValueIR) -> OperandAssembly {
    match value {
        ValueIR::ConstantIR(val) => OperandAssembly::Immediate(*val),
        ValueIR::VariableIR(name) => OperandAssembly::PseudoRegister(name.clone()),
    }
}

// Process first AST to replace pseudoregisters with stack addresses and then allocate stack space

/// Replaces pseudoregisters in a function definition with stack addresses,
/// allocates stack space, and fixes invalid move instructions.
///
/// This function performs three main transformations:
/// 1. Replaces all pseudoregisters with stack variable offsets
/// 2. Inserts a stack allocation instruction at the beginning of the function
/// 3. Fixes invalid move instructions where both operands are stack variables
///
/// # Arguments
///
/// * `function_def` - The function definition containing pseudoregisters to replace.
///
/// # Returns
///
/// Returns the updated `FunctionDefinitionAssembly` with all pseudoregisters replaced,
/// stack space allocated, and all move instructions valid.
///
/// # Errors
///
/// Returns a `CodeGenError` if any transformation step fails.
fn replace_pseudoregisters(
    function_def: FunctionDefinitionAssembly,
) -> Result<FunctionDefinitionAssembly, CodeGenError> {
    let (ast_with_stack_addresses, stack_offset) =
        replace_pseudoregisters_with_stack_address(function_def);
    // Add the stack allocation instruction at the beginning of the function to allocate space for all stack variables.
    let allocated_function = allocate_function_stack_space(ast_with_stack_addresses, stack_offset);
    // Fix broken Mov instructions
    Ok(fix_broken_moves(allocated_function))
}

fn replace_pseudoregisters_with_stack_address(
    function_def: FunctionDefinitionAssembly,
) -> (FunctionDefinitionAssembly, i32) {
    match function_def {
        FunctionDefinitionAssembly::Function {
            identifier,
            instructions,
        } => {
            let mut stack_offset = 0;
            let mut identifier_with_stack_offset: HashMap<String, i32> = HashMap::new();
            let mut updated_instructions = Vec::new();

            for instr in instructions {
                match instr {
                    InstructionAssembly::Mov { src, dst } => {
                        let updated_src = match src {
                            OperandAssembly::PseudoRegister(name) => {
                                replace_pseudoregister_with_stack_address(
                                    &name,
                                    &mut stack_offset,
                                    &mut identifier_with_stack_offset,
                                )
                            }
                            _ => src,
                        };
                        let updated_dst = match dst {
                            OperandAssembly::PseudoRegister(name) => {
                                replace_pseudoregister_with_stack_address(
                                    &name,
                                    &mut stack_offset,
                                    &mut identifier_with_stack_offset,
                                )
                            }
                            _ => dst,
                        };
                        updated_instructions.push(InstructionAssembly::Mov {
                            src: updated_src,
                            dst: updated_dst,
                        });
                    }
                    InstructionAssembly::Unary {
                        unary_operator,
                        operand,
                    } => {
                        let updated_operand = match operand {
                            OperandAssembly::PseudoRegister(name) => {
                                replace_pseudoregister_with_stack_address(
                                    &name,
                                    &mut stack_offset,
                                    &mut identifier_with_stack_offset,
                                )
                            }
                            _ => operand,
                        };
                        updated_instructions.push(InstructionAssembly::Unary {
                            unary_operator,
                            operand: updated_operand,
                        });
                    }
                    _ => updated_instructions.push(instr),
                }
            }
            (
                FunctionDefinitionAssembly::Function {
                    identifier,
                    instructions: updated_instructions,
                },
                stack_offset,
            )
        }
    }
}

fn replace_pseudoregister_with_stack_address(
    name: &str,
    stack_offset: &mut i32,
    identifier_with_stack_offset: &mut HashMap<String, i32>,
) -> OperandAssembly {
    if let Some(offset) = identifier_with_stack_offset.get(name) {
        return OperandAssembly::StackVariable(*offset);
    }
    *stack_offset -= 4; // 4 bytes per variable
    identifier_with_stack_offset.insert(name.to_string(), *stack_offset);
    OperandAssembly::StackVariable(*stack_offset)
}

fn allocate_function_stack_space(
    function: FunctionDefinitionAssembly,
    stack_offset: i32,
) -> FunctionDefinitionAssembly {
    let FunctionDefinitionAssembly::Function {
        identifier,
        mut instructions,
    } = function;

    instructions.insert(0, InstructionAssembly::AllocateStack { size: stack_offset }); // todo: O(n) because it is Vec and not VecDeque

    FunctionDefinitionAssembly::Function {
        identifier,
        instructions,
    }
}

// After replacing the pseudoregisters  with stack addresses, it is possible to have a move instruction with memory addresses as both operands. This is invalid, an it has to be replaced by two moves with an auxiliary register
fn fix_broken_moves(function: FunctionDefinitionAssembly) -> FunctionDefinitionAssembly {
    let FunctionDefinitionAssembly::Function {
        identifier,
        mut instructions,
    } = function;
    let mut updated_instructions: Vec<InstructionAssembly> = Vec::with_capacity(instructions.len());

    for instruction in instructions.drain(..) {
        match &instruction {
            // Both operands are stack variables, use auxiliary register (R10) with two moves
            InstructionAssembly::Mov {
                src: src_operand @ OperandAssembly::StackVariable(_),
                dst: dst_operand @ OperandAssembly::StackVariable(_),
            } => {
                let aux_reg = OperandAssembly::Register(RegisterAssembly::R10);

                updated_instructions.extend([
                    InstructionAssembly::Mov {
                        src: src_operand.clone(),
                        dst: aux_reg.clone(),
                    },
                    InstructionAssembly::Mov {
                        src: aux_reg,
                        dst: dst_operand.clone(),
                    },
                ]);
            }
            // Any other instruction — keep as is
            _ => updated_instructions.push(instruction),
        }
    }

    FunctionDefinitionAssembly::Function {
        identifier,
        instructions: updated_instructions,
    }
}
