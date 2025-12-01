use std::collections::HashMap;

use crate::backend::{
    assembly_definition::{
        BinaryOperatorAssembly, FunctionDefinitionAssembly, InstructionAssembly, InstructionSize, OperandAssembly, ProgramAssembly, RegisterAssembly, UnaryOperatorAssembly
    },
    intermediate_representation::ir_definition::{
        BinaryOperatorIR, FunctionDefinitionIR, InstructionIR, ProgramIR, UnaryOperatorIR, ValueIR,
    },
};

pub fn generate_code(ast: ProgramIR) -> ProgramAssembly {
    let ast_with_temporal_addresses = match ast {
        ProgramIR::ProgramIR(func_def) => {
            ProgramAssembly::ProgramAssembly(process_function_definition(func_def))
        }
    };

    // Right now there is only one function per program, so we can process it directly. Replace it with a loop later if needed.
    let ProgramAssembly::ProgramAssembly(func_def) = ast_with_temporal_addresses;
    let function_definition_with_stack_addresses = fix_assembly_ast(func_def);

    ProgramAssembly::ProgramAssembly(function_definition_with_stack_addresses)
}

// ===============================================
// Process IR to Assembly AST
// ===============================================

fn process_function_definition(func: FunctionDefinitionIR) -> FunctionDefinitionAssembly {
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

fn process_instructions(instructions_ir: Vec<InstructionIR>) -> Vec<InstructionAssembly> {
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
            InstructionIR::BinaryIR {
                binary_operator,
                left_operand,
                right_operand,
                dst,
            } => assembly_ins.extend(process_binary_instruction(
                binary_operator,
                left_operand,
                right_operand,
                dst,
            )),
        }
    }
    assembly_ins
}

fn process_unary_instruction(
    unary_operator: UnaryOperatorIR,
    src: ValueIR,
    dst: ValueIR,
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
        size: InstructionSize::DoubleWord,
    });
    instructions.push(InstructionAssembly::Unary {
        unary_operator: operator,
        operand: dst_operand,
    });
    instructions
}

fn process_binary_instruction(
    binary_operator: BinaryOperatorIR,
    left_operand: ValueIR,
    right_operand: ValueIR,
    dst: ValueIR,
) -> Vec<InstructionAssembly> {
    let mut instructions = Vec::new();
    let operator = match binary_operator {
        BinaryOperatorIR::AdditionIR => BinaryOperatorAssembly::AdditionAssembly,
        BinaryOperatorIR::SubtractionIR => BinaryOperatorAssembly::SubtractionAssembly,
        BinaryOperatorIR::MultiplicationIR => BinaryOperatorAssembly::MultiplicationAssembly,
        BinaryOperatorIR::BitwiseAndIR => BinaryOperatorAssembly::BitwiseAndAssembly,
        BinaryOperatorIR::BitwiseOrIR => BinaryOperatorAssembly::BitwiseOrAssembly,
        BinaryOperatorIR::BitwiseXorIR => BinaryOperatorAssembly::BitwiseXorAssembly,
        BinaryOperatorIR::ShiftLeftIR => BinaryOperatorAssembly::ShiftLeftAssembly,
        BinaryOperatorIR::ShiftRightIR => BinaryOperatorAssembly::ShiftRightAssembly,
        BinaryOperatorIR::DivisionIR => {
            return process_division_instruction(left_operand, right_operand, dst);
        }
        BinaryOperatorIR::ModulusIR => {
            return process_modulo_instruction(left_operand, right_operand, dst);
        }
    };
    let left_operand = process_value(left_operand);
    let right_operand = process_value(right_operand);
    let dst_operand = process_value(dst);

    // Important: In x86 binary are calculated as dst = dst (right operand) op src (left operand). So we need to flip the operands and move the left operand to dst first
    // Example: sub dst, src  => dst = dst - src. So Tacky Add left, right, dst becomes mov left, dst ; sub right, dst
    instructions.push(InstructionAssembly::Mov {
        // In x86 assembly the result is stored in the right operand
        src: left_operand,
        dst: dst_operand.clone(),
        size: InstructionSize::DoubleWord,
    });
    instructions.push(InstructionAssembly::Binary {
        binary_operator: operator,
        left_operand: right_operand,
        right_operand: dst_operand.clone(),
    });
    instructions
}

// Division and modulus share common steps, only the final move differs as two different registers are used for the remainder and the quotient (AX for quotient, DX for remainder)
fn process_div_mod_common(
    dividend: ValueIR,
    divisor: ValueIR,
    dst: ValueIR,
    result_reg: RegisterAssembly, // AX for quotient (div), DX for remainder (mod)
) -> Vec<InstructionAssembly> {
    let mut instructions = Vec::new();

    // Move dividend into AX
    instructions.push(InstructionAssembly::Mov {
        src: process_value(dividend),
        dst: OperandAssembly::Register(RegisterAssembly::Ax),
        size: InstructionSize::DoubleWord,
    });

    // Sign-extend EAX into EDX:EAX
    instructions.push(InstructionAssembly::Cdq);

    // Perform integer division
    instructions.push(InstructionAssembly::Idiv {
        divisor: process_value(divisor),
    });

    // Move either AX (quotient) or DX (remainder) to destination
    instructions.push(InstructionAssembly::Mov {
        src: OperandAssembly::Register(result_reg),
        dst: process_value(dst),
        size: InstructionSize::DoubleWord,
    });

    instructions
}

fn process_division_instruction(
    dividend: ValueIR,
    divisor: ValueIR,
    dst: ValueIR,
) -> Vec<InstructionAssembly> {
    process_div_mod_common(dividend, divisor, dst, RegisterAssembly::Ax)
}

fn process_modulo_instruction(
    dividend: ValueIR,
    divisor: ValueIR,
    dst: ValueIR,
) -> Vec<InstructionAssembly> {
    process_div_mod_common(dividend, divisor, dst, RegisterAssembly::Dx)
}

fn process_return_instruction(value: ValueIR) -> Vec<InstructionAssembly> {
    let mut instructions = Vec::new();
    let src_operand = process_value(value);
    instructions.push(InstructionAssembly::Mov {
        src: src_operand,
        dst: OperandAssembly::Register(RegisterAssembly::Ax),
        size: InstructionSize::DoubleWord,
    }); // Placeholder for MOV instruction to move return value to appropriate register
    instructions.push(InstructionAssembly::Ret);
    instructions
}

fn process_value(value: ValueIR) -> OperandAssembly {
    match value {
        ValueIR::ConstantIR(val) => OperandAssembly::Immediate(val),
        ValueIR::VariableIR(name) => OperandAssembly::PseudoRegister(name),
    }
}

// ==============================================
// Fixes for generated Assembly AST.
// Memory Allocation and invalid instruction fixes
// ==============================================

/// Traverser the assembly AST and performs necessary fixes to ensure valid x86 assembly code.
///
/// # Returns
/// Returns the fixed `FunctionDefinitionAssembly`
fn fix_assembly_ast(function_def: FunctionDefinitionAssembly) -> FunctionDefinitionAssembly {
    let (ast_with_stack_addresses, stack_offset) = replace_pseudoregisters(function_def);
    let allocated_function = allocate_function_stack_space(ast_with_stack_addresses, stack_offset);
    fix_invalid_instructions(allocated_function)
}

// Replace pseudoregisters with stack variable addresses
fn replace_pseudoregisters(
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
                    InstructionAssembly::Mov { src, dst, size } => {
                        let updated_src = replace_operand_pseudoregister(
                            src,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        let updated_dst = replace_operand_pseudoregister(
                            dst,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        updated_instructions.push(InstructionAssembly::Mov {
                            src: updated_src,
                            dst: updated_dst,
                            size: size,
                        });
                    }
                    InstructionAssembly::Unary {
                        unary_operator,
                        operand,
                    } => {
                        let updated_operand = replace_operand_pseudoregister(
                            operand,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        updated_instructions.push(InstructionAssembly::Unary {
                            unary_operator,
                            operand: updated_operand,
                        });
                    }
                    InstructionAssembly::Binary {
                        binary_operator,
                        left_operand,
                        right_operand,
                    } => {
                        let updated_left_operand = replace_operand_pseudoregister(
                            left_operand,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        let updated_right_operand = replace_operand_pseudoregister(
                            right_operand,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        updated_instructions.push(InstructionAssembly::Binary {
                            binary_operator,
                            left_operand: updated_left_operand,
                            right_operand: updated_right_operand,
                        });
                    }
                    InstructionAssembly::Idiv { divisor } => {
                        let updated_divisor = replace_operand_pseudoregister(
                            divisor,
                            &mut stack_offset,
                            &mut identifier_with_stack_offset,
                        );
                        updated_instructions.push(InstructionAssembly::Idiv {
                            divisor: updated_divisor,
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

// If the operand is a pseudoregister replaces it with a stack variable address or returns. If the pseudoregister has not been seen before, it assigns a new stack offset.
fn replace_operand_pseudoregister(
    operand: OperandAssembly,
    stack_offset: &mut i32,
    identifier_with_stack_offset: &mut HashMap<String, i32>,
) -> OperandAssembly {
    if let OperandAssembly::PseudoRegister(name) = operand {
        if let Some(offset) = identifier_with_stack_offset.get(&name) {
            return OperandAssembly::StackVariable(*offset); // Make it negative to address from RBP downwards
        }
        *stack_offset += 4; // 4 bytes per variable
        identifier_with_stack_offset.insert(name.to_string(), *stack_offset);
        OperandAssembly::StackVariable(*stack_offset)
    } else {
        operand
    }
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

// Fixes instructions that are invalid in x86 assembly
fn fix_invalid_instructions(function: FunctionDefinitionAssembly) -> FunctionDefinitionAssembly {
    let FunctionDefinitionAssembly::Function {
        identifier,
        mut instructions,
    } = function;
    let mut updated_instructions: Vec<InstructionAssembly> = Vec::with_capacity(instructions.len());

    for instruction in instructions.drain(..) {
        match instruction {
            // Mov instructions with two stack variables as operands that are invalid in x86 assembly. Use an auxiliary register to perform the move
            InstructionAssembly::Mov {
                src: src_operand @ OperandAssembly::StackVariable(_),
                dst: dst_operand @ OperandAssembly::StackVariable(_),
                size,
            } => {
                let aux_reg = OperandAssembly::Register(RegisterAssembly::R10);

                updated_instructions.extend([
                    InstructionAssembly::Mov {
                        src: src_operand.clone(),
                        dst: aux_reg.clone(),
                        size: size.clone(),
                    },
                    InstructionAssembly::Mov {
                        src: aux_reg,
                        dst: dst_operand.clone(),
                        size: size,
                    },
                ]);
            }
            // Shift instructions need to use the CL register for the count when the count is variable
            InstructionAssembly::Binary {
                binary_operator:
                    binary_operator @ (BinaryOperatorAssembly::ShiftLeftAssembly
                    | BinaryOperatorAssembly::ShiftRightAssembly),
                left_operand: count,
                right_operand: destination,
            } if !matches!(count, OperandAssembly::Register(RegisterAssembly::Cl) | OperandAssembly::Immediate(_)) => {
                let cl_reg = OperandAssembly::Register(RegisterAssembly::Cl);

                // Move count to CL
                updated_instructions.push(InstructionAssembly::Mov {
                    src: count,
                    dst: cl_reg.clone(),
                    size: InstructionSize::Byte, // CL is an 8-bit register
                });

                // Perform shift using CL
                updated_instructions.push(InstructionAssembly::Binary {
                    binary_operator,
                    left_operand: cl_reg,
                    right_operand: destination
                });
            }
            // Binary instructions (except shifts and multiplication, that have special requirements) with two stack variables as operands that are invalid in x86 assembly. Move left operand to auxiliary register first
            InstructionAssembly::Binary {
                binary_operator,
                left_operand: left_operand @ OperandAssembly::StackVariable(_),
                right_operand: right_operand @ OperandAssembly::StackVariable(_),
            } if !matches!(
                binary_operator,
                BinaryOperatorAssembly::ShiftLeftAssembly
                    | BinaryOperatorAssembly::ShiftRightAssembly
                    | BinaryOperatorAssembly::MultiplicationAssembly
            ) => {
                let aux_reg = OperandAssembly::Register(RegisterAssembly::R10);

                updated_instructions.extend([
                    InstructionAssembly::Mov {
                        src: left_operand.clone(),
                        dst: aux_reg.clone(),
                        size: InstructionSize::DoubleWord,
                    },
                    InstructionAssembly::Binary {
                        binary_operator: binary_operator,
                        left_operand: aux_reg,
                        right_operand: right_operand,
                    },
                ]);
            }
            // Mul instruction can't use a memory address as the destination. Move to an auxiliary register first and then back
            InstructionAssembly::Binary {
                binary_operator: BinaryOperatorAssembly::MultiplicationAssembly,
                left_operand,
                right_operand: right_operand @ OperandAssembly::StackVariable(_),
            } => {
                let aux_reg = OperandAssembly::Register(RegisterAssembly::R11);

                updated_instructions.extend([
                    InstructionAssembly::Mov {
                        src: right_operand.clone(),
                        dst: aux_reg.clone(),
                        size: InstructionSize::DoubleWord,
                    },
                    InstructionAssembly::Binary {
                        binary_operator: BinaryOperatorAssembly::MultiplicationAssembly,
                        left_operand: left_operand,
                        right_operand: aux_reg.clone(),
                    },
                    InstructionAssembly::Mov {
                        src: aux_reg,
                        dst: right_operand,
                        size: InstructionSize::DoubleWord,
                    },
                ]);
            }
            // Idiv instruction can't use an immediate as divisor. Move to an auxiliary register first
            InstructionAssembly::Idiv {
                divisor: divisor @ OperandAssembly::Immediate(_),
            } => {
                updated_instructions.push(InstructionAssembly::Mov {
                    src: divisor,
                    dst: OperandAssembly::Register(RegisterAssembly::R10),
                    size: InstructionSize::DoubleWord,
                });
                updated_instructions.push(InstructionAssembly::Idiv {
                    divisor: OperandAssembly::Register(RegisterAssembly::R10),
                });
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
