use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;
use std::fmt;

use binaryninja::{
    disassembly::{InstructionTextToken, InstructionTextTokenContents},
    string::BnString,
};

pub trait ToTokens {
    fn to_tokens(&self, addr: u64) -> Vec<InstructionTextToken>;
}

#[derive(Copy, Clone)]
pub enum Instruction {
    Nop,
    Ld(LdType),

    Add(AluSrc),
    Adc(AluSrc),
    Sub(AluSrc),
    Sbc(AluSrc),
    And(AluSrc),
    Xor(AluSrc),
    Or(AluSrc),
    Cp(AluSrc),

    IncR8(R8),
    DecR8(R8),
    IncR16(R16),
    DecR16(R16),

    AddHL(R16),
    AddSP(i8),

    Rlc(R8),
    Rrc(R8),
    Rl(R8),
    Rr(R8),
    Sla(R8),
    Sra(R8),
    Swap(R8),
    Srl(R8),
    Bit(BitPos, R8),
    Res(BitPos, R8),
    Set(BitPos, R8),

    Rlca,
    Rla,
    Rrca,
    Rra,

    Jr(Option<BranchCond>, i8),
    Jp(Option<BranchCond>, u16),
    Call(Option<BranchCond>, u16),
    Ret(Option<BranchCond>),
    Rst(u8),
    Reti,
    JpHL,

    Push(PushPop),
    Pop(PushPop),

    Scf,
    Ccf,

    Daa,
    Cpl,

    Stop,
    Halt,

    Di,
    Ei,

    Illegal,
}

impl Instruction {
    pub fn decode(data: &[u8]) -> Option<Self> {
        let byte = data.get(0)?;
        let lo_3bit = byte & 0b111;
        let hi_3bit = (byte & 0b111111) >> 3;
        let hi_2bit = hi_3bit >> 1;
        let r8_lo = R8::from_u8(lo_3bit)?;
        let r8_hi = R8::from_u8(hi_3bit)?;
        let r16 = R16::from_u8(hi_2bit)?;
        let ind = Indirect::from_u8(hi_2bit)?;
        let branch = BranchCond::from_u8(hi_3bit & 0b11)?;
        let push_pop = PushPop::from_u8(hi_2bit)?;
        Some(match byte {
            0x00 => Instruction::Nop,
            0x01 | 0x11 | 0x21 | 0x31 => Instruction::Ld(LdType::R16Imm(r16, u16_arg(data)?)),
            0x02 | 0x12 | 0x22 | 0x32 => Instruction::Ld(LdType::IndFromA(ind)),
            0x03 | 0x13 | 0x23 | 0x33 => Instruction::IncR16(r16),
            0x04 | 0x14 | 0x24 | 0x34 | 0x0c | 0x1c | 0x2c | 0x3c => Instruction::IncR8(r8_hi),
            0x05 | 0x15 | 0x25 | 0x35 | 0x0d | 0x1d | 0x2d | 0x3d => Instruction::DecR8(r8_hi),
            0x06 | 0x16 | 0x26 | 0x36 | 0x0e | 0x1e | 0x2e | 0x3e => {
                Instruction::Ld(LdType::R8Imm(r8_hi, data[1]))
            }
            0x08 => Instruction::Ld(LdType::StoreSP(u16_arg(data)?)),
            0x09 | 0x19 | 0x29 | 0x39 => Instruction::AddHL(r16),
            0x0a | 0x1a | 0x2a | 0x3a => Instruction::Ld(LdType::AFromInd(ind)),
            0x0b | 0x1b | 0x2b | 0x3b => Instruction::DecR16(r16),
            0x20 | 0x28 | 0x30 | 0x38 => Instruction::Jr(Some(branch), u8_arg(data)? as i8),
            0x40..=0x75 | 0x77..=0x7f => Instruction::Ld(LdType::R8(r8_hi, r8_lo)),
            0x80..=0xbf => Self::decode_alu(*byte, AluSrc::R8(r8_lo)),
            0xc0 | 0xd0 | 0xc8 | 0xd8 => Instruction::Ret(Some(branch)),
            0xc1 | 0xd1 | 0xe1 | 0xf1 => Instruction::Pop(push_pop),
            0xc2 | 0xd2 | 0xca | 0xda => Instruction::Jp(Some(branch), u16_arg(data)?),
            0xc4 | 0xd4 | 0xcc | 0xdc => Instruction::Call(Some(branch), u16_arg(data)?),
            0xc5 | 0xd5 | 0xe5 | 0xf5 => Instruction::Push(push_pop),
            0xc6 | 0xd6 | 0xe6 | 0xf6 | 0xce | 0xde | 0xee | 0xfe => {
                Self::decode_alu(*byte, AluSrc::Imm(u8_arg(data)?))
            }
            0xc7 | 0xd7 | 0xe7 | 0xf7 | 0xcf | 0xdf | 0xef | 0xff => Instruction::Rst(hi_3bit << 3),
            0xcb => {
                let second_byte = u8_arg(data)?;
                let bit = BitPos::from_u8((second_byte & 0b111111) >> 3).unwrap();
                let r8 = R8::from_u8(second_byte & 0b111).unwrap();
                match second_byte {
                    0x00..=0x07 => Instruction::Rlc(r8),
                    0x08..=0x0f => Instruction::Rrc(r8),
                    0x10..=0x17 => Instruction::Rl(r8),
                    0x18..=0x1f => Instruction::Rr(r8),
                    0x20..=0x27 => Instruction::Sla(r8),
                    0x28..=0x2f => Instruction::Sra(r8),
                    0x30..=0x37 => Instruction::Swap(r8),
                    0x38..=0x3f => Instruction::Srl(r8),
                    0x40..=0x7f => Instruction::Bit(bit, r8),
                    0x80..=0xbf => Instruction::Res(bit, r8),
                    0xc0..=0xff => Instruction::Set(bit, r8),
                }
            }
            0xe0 => Instruction::Ld(LdType::IoRegFromA(Io::Imm(u8_arg(data)?))),
            0xe2 => Instruction::Ld(LdType::IoRegFromA(Io::C)),
            0xea => Instruction::Ld(LdType::MemFromA(u16_arg(data)?)),
            0xf0 => Instruction::Ld(LdType::AFromIoReg(Io::Imm(u8_arg(data)?))),
            0xf2 => Instruction::Ld(LdType::AFromIoReg(Io::C)),
            0xfa => Instruction::Ld(LdType::AFromMem(u16_arg(data)?)),

            0x07 => Instruction::Rlca,
            0x0f => Instruction::Rrca,
            0x10 => Instruction::Stop,
            0x17 => Instruction::Rla,
            0x18 => Instruction::Jr(None, u8_arg(data)? as i8),
            0x1f => Instruction::Rra,
            0x27 => Instruction::Daa,
            0x2f => Instruction::Cpl,
            0x37 => Instruction::Scf,
            0x3f => Instruction::Ccf,
            0x76 => Instruction::Halt,
            0xc3 => Instruction::Jp(None, u16_arg(data)?),
            0xc9 => Instruction::Ret(None),
            0xcd => Instruction::Call(None, u16_arg(data)?),
            0xd9 => Instruction::Reti,
            0xe8 => Instruction::AddSP(u8_arg(data)? as i8),
            0xe9 => Instruction::JpHL,
            0xf3 => Instruction::Di,
            0xfb => Instruction::Ei,
            0xf8 => Instruction::Ld(LdType::HLFromSP(u8_arg(data)? as i8)),
            0xf9 => Instruction::Ld(LdType::SPFromHL),

            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4 | 0xfc | 0xfd => {
                Instruction::Illegal
            }
        })
    }

    fn decode_alu(byte: u8, src: AluSrc) -> Self {
        match (byte & 0b111111) >> 3 {
            0 => Instruction::Add(src),
            1 => Instruction::Adc(src),
            2 => Instruction::Sub(src),
            3 => Instruction::Sbc(src),
            4 => Instruction::And(src),
            5 => Instruction::Xor(src),
            6 => Instruction::Or(src),
            7 => Instruction::Cp(src),
            _ => unreachable!(),
        }
    }

    pub const fn length(self) -> usize {
        use Instruction::*;
        match self {
            Nop => 1,
            Ld(ld_type) => match ld_type {
                LdType::R8(_, _) => 1,
                LdType::R8Imm(_, _) => 2,
                LdType::R16Imm(_, _) => 3,
                LdType::AFromInd(_) | LdType::IndFromA(_) => 1,
                LdType::AFromIoReg(io) | LdType::IoRegFromA(io) => match io {
                    Io::C => 1,
                    Io::Imm(_) => 2,
                },
                LdType::AFromMem(_) | LdType::MemFromA(_) => 3,
                LdType::StoreSP(_) => 3,
                LdType::HLFromSP(_) => 2,
                LdType::SPFromHL => 1,
            },

            Add(src) | Adc(src) | Sub(src) | Sbc(src) | And(src) | Xor(src) | Or(src) | Cp(src) => {
                match src {
                    AluSrc::R8(_) => 1,
                    AluSrc::Imm(_) => 2,
                }
            }

            IncR8(_) | DecR8(_) | IncR16(_) | DecR16(_) => 1,

            AddHL(_) => 1,
            AddSP(_) => 2,

            Rlc(_)
            | Rrc(_)
            | Rl(_)
            | Rr(_)
            | Sla(_)
            | Sra(_)
            | Swap(_)
            | Srl(_)
            | Bit(_, _)
            | Res(_, _)
            | Set(_, _) => 2,

            Rlca | Rla | Rrca | Rra => 1,

            Jr(_, _) => 2,
            Jp(_, _) => 3,
            Call(_, _) => 3,
            Ret(_) => 1,
            Rst(_) => 1,
            Reti => 1,
            JpHL => 1,

            Push(_) | Pop(_) => 1,

            Scf | Ccf => 1,

            Daa => 1,
            Cpl => 1,

            Stop | Halt => 1,

            Di | Ei => 1,

            Illegal => 1,
        }
    }

    pub fn mnemonic(&self) -> &'static str {
        use Instruction::*;
        match self {
            Nop => "NOP",
            Ld(ld_type) => match ld_type {
                LdType::AFromIoReg(_) | LdType::IoRegFromA(_) => "LDH",
                _ => "LD",
            },

            Add(_) | AddHL(_) | AddSP(_) => "ADD",
            Adc(_) => "ADC",
            Sub(_) => "SUB",
            Sbc(_) => "SBC",
            And(_) => "AND",
            Xor(_) => "XOR",
            Or(_) => "OR",
            Cp(_) => "CP",

            IncR8(_) | IncR16(_) => "INC",
            DecR8(_) | DecR16(_) => "DEC",

            Rlc(_) => "RLC",
            Rrc(_) => "RRC",
            Rl(_) => "RL",
            Rr(_) => "RR",
            Sla(_) => "SLA",
            Sra(_) => "SRA",
            Swap(_) => "SWAP",
            Srl(_) => "SRL",
            Bit(_, _) => "BIT",
            Res(_, _) => "RES",
            Set(_, _) => "SET",

            Rlca => "RLCA",
            Rla => "RLA",
            Rrca => "RRCA",
            Rra => "RRA",

            Jr(_, _) => "JR",
            Jp(_, _) | JpHL => "JP",
            Call(_, _) => "CALL",
            Ret(_) => "RET",
            Rst(_) => "RST",
            Reti => "RETI",

            Push(_) => "PUSH",
            Pop(_) => "POP",

            Scf => "SCF",
            Ccf => "CCF",

            Daa => "DAA",
            Cpl => "Cpl",

            Stop => "STOP",
            Halt => "HALT",

            Di => "DI",
            Ei => "EI",

            Illegal => "??",
        }
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Nop => write!(f, "NOP"),
            Ld(ld_type) => match ld_type {
                LdType::R8(dest, src) => write!(f, "LD {dest:?}, {src:?}"),
                LdType::R8Imm(dest, val) => write!(f, "LD {dest:?}, ${val:02x}"),
                LdType::R16Imm(dest, val) => write!(f, "LD {dest:?}, ${val:04x}"),
                LdType::AFromInd(ind) => write!(f, "LD A, {ind:?}"),
                LdType::IndFromA(ind) => write!(f, "LD {ind:?}, A"),
                LdType::AFromIoReg(io) => match io {
                    Io::C => write!(f, "LDH A, (FF00 + C)"),
                    Io::Imm(val) => write!(f, "LDH A, ($FF00 + ${val:02x})"),
                },
                LdType::IoRegFromA(io) => match io {
                    Io::C => write!(f, "LDH (FF00 + C), A"),
                    Io::Imm(val) => write!(f, "LDH ($FF00 + ${val:02x}), A"),
                },
                LdType::AFromMem(val) => write!(f, "LD A, (${val:04x})"),
                LdType::MemFromA(val) => write!(f, "LD (${val:04x}), A"),
                LdType::StoreSP(val) => write!(f, "LD (${val:04x}), SP"),
                LdType::HLFromSP(val) => write!(f, "LD HL, SP{val:+}"),
                LdType::SPFromHL => write!(f, "LD SP, HL"),
            },

            Add(src) => write!(f, "ADD A, {src:?}"),
            Adc(src) => write!(f, "ADC A, {src:?}"),
            Sub(src) => write!(f, "SUB A, {src:?}"),
            Sbc(src) => write!(f, "SBC A, {src:?}"),
            And(src) => write!(f, "AND A, {src:?}"),
            Xor(src) => write!(f, "XOR A, {src:?}"),
            Or(src) => write!(f, "OR A, {src:?}"),
            Cp(src) => write!(f, "CP A, {src:?}"),

            IncR8(r8) => write!(f, "INC {r8:?}"),
            DecR8(r8) => write!(f, "DEC {r8:?}"),
            IncR16(r16) => write!(f, "INC {r16:?}"),
            DecR16(r16) => write!(f, "DEC {r16:?}"),

            AddHL(r16) => write!(f, "ADD HL, {r16:?}"),
            AddSP(val) => write!(f, "ADD SP, {val:+}"),

            Rlc(r8) => write!(f, "RLC {r8:?}"),
            Rrc(r8) => write!(f, "RRC {r8:?}"),
            Rl(r8) => write!(f, "RL {r8:?}"),
            Rr(r8) => write!(f, "RR {r8:?}"),
            Sla(r8) => write!(f, "SLA {r8:?}"),
            Sra(r8) => write!(f, "SRA {r8:?}"),
            Swap(r8) => write!(f, "SWAP {r8:?}"),
            Srl(r8) => write!(f, "SRL {r8:?}"),
            Bit(bit, r8) => write!(f, "BIT {bit:?}, {r8:?}"),
            Res(bit, r8) => write!(f, "RES {bit:?}, {r8:?}"),
            Set(bit, r8) => write!(f, "SET {bit:?}, {r8:?}"),

            Rlca => write!(f, "RLCA"),
            Rla => write!(f, "RLA"),
            Rrca => write!(f, "RRCA"),
            Rra => write!(f, "RRA"),

            Jr(Some(cond), val) => write!(f, "JR {cond:?}, {val:+}"),
            Jr(None, val) => write!(f, "JR {val:+}"),
            Jp(Some(cond), val) => write!(f, "JP {cond:?}, {val:04X}"),
            Jp(None, val) => write!(f, "JP {val:04X}"),
            Call(Some(cond), val) => write!(f, "CALL {cond:?}, {val:04X}"),
            Call(None, val) => write!(f, "CALL {val:04X}"),
            Ret(Some(cond)) => write!(f, "RET {cond:?}"),
            Ret(None) => write!(f, "RET"),
            Rst(val) => write!(f, "RST ${val:02x}"),
            Reti => write!(f, "RETI"),
            JpHL => write!(f, "JP HL"),

            Push(src) => write!(f, "PUSH {src:?}"),
            Pop(dest) => write!(f, "POP {dest:?}"),

            Scf => write!(f, "SCF"),
            Ccf => write!(f, "CCF"),

            Daa => write!(f, "DAA"),
            Cpl => write!(f, "CPL"),

            Stop => write!(f, "STOP"),
            Halt => write!(f, "HALT"),

            Di => write!(f, "DI"),
            Ei => write!(f, "EI"),

            Illegal => write!(f, "??"),
        }
    }
}

impl ToTokens for Instruction {
    fn to_tokens(&self, addr: u64) -> Vec<InstructionTextToken> {
        use Instruction::*;

        let num_spaces = 8 - self.mnemonic().len();
        let mut spaces = Vec::with_capacity(num_spaces);
        for _ in 0..num_spaces {
            spaces.push(InstructionTextToken::new(
                BnString::new(" "),
                InstructionTextTokenContents::Text,
            ));
        }

        let separator = vec![
            InstructionTextToken::new(
                BnString::new(","),
                InstructionTextTokenContents::OperandSeparator,
            ),
            InstructionTextToken::new(BnString::new(" "), InstructionTextTokenContents::Text),
        ];

        let mut tokens = vec![InstructionTextToken::new(
            BnString::new(self.mnemonic()),
            InstructionTextTokenContents::Instruction,
        )];

        match self {
            Ld(ld_type) => {
                tokens.extend(spaces.clone());
                match ld_type {
                    LdType::R8(dest, src) => {
                        tokens.extend(dest.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(src.to_tokens(addr));
                    }
                    LdType::R8Imm(dest, val) => {
                        tokens.extend(dest.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.push(InstructionTextToken::new(
                            BnString::new(format!("{val:#04X}")),
                            InstructionTextTokenContents::Integer(*val as u64),
                        ));
                    }
                    LdType::R16Imm(dest, val) => {
                        tokens.extend(dest.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.push(InstructionTextToken::new(
                            BnString::new(format!("{val:#06X}")),
                            InstructionTextTokenContents::Integer(*val as u64),
                        ));
                    }
                    LdType::AFromInd(ind) => {
                        tokens.extend(R8::A.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(ind.to_tokens(addr));
                    }
                    LdType::IndFromA(ind) => {
                        tokens.extend(ind.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(R8::A.to_tokens(addr));
                    }
                    LdType::AFromIoReg(io) => {
                        tokens.extend(R8::A.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(io.to_tokens(addr));
                    }
                    LdType::IoRegFromA(io) => {
                        tokens.extend(io.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(R8::A.to_tokens(addr));
                    }
                    LdType::AFromMem(val) => {
                        tokens.extend(R8::A.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(vec![
                            InstructionTextToken::new(
                                BnString::new("("),
                                InstructionTextTokenContents::BeginMemoryOperand,
                            ),
                            InstructionTextToken::new(
                                BnString::new(format!("{val:#06X}")),
                                InstructionTextTokenContents::PossibleAddress(*val as u64),
                            ),
                            InstructionTextToken::new(
                                BnString::new(")"),
                                InstructionTextTokenContents::EndMemoryOperand,
                            ),
                        ]);
                    }
                    LdType::MemFromA(val) => {
                        tokens.extend(vec![
                            InstructionTextToken::new(
                                BnString::new("("),
                                InstructionTextTokenContents::BeginMemoryOperand,
                            ),
                            InstructionTextToken::new(
                                BnString::new(format!("{val:#06X}")),
                                InstructionTextTokenContents::PossibleAddress(*val as u64),
                            ),
                            InstructionTextToken::new(
                                BnString::new(")"),
                                InstructionTextTokenContents::EndMemoryOperand,
                            ),
                        ]);
                        tokens.extend(separator.clone());
                        tokens.extend(R8::A.to_tokens(addr));
                    }
                    LdType::StoreSP(val) => {
                        tokens.extend(vec![
                            InstructionTextToken::new(
                                BnString::new("("),
                                InstructionTextTokenContents::BeginMemoryOperand,
                            ),
                            InstructionTextToken::new(
                                BnString::new(format!("{val:#06X}")),
                                InstructionTextTokenContents::PossibleAddress(*val as u64),
                            ),
                            InstructionTextToken::new(
                                BnString::new(")"),
                                InstructionTextTokenContents::EndMemoryOperand,
                            ),
                        ]);
                        tokens.extend(separator.clone());
                        tokens.extend(R16::SP.to_tokens(addr));
                    }
                    LdType::HLFromSP(val) => {
                        tokens.extend(R16::HL.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(R16::SP.to_tokens(addr));
                        let sign = if *val < 0 {
                            Some("-")
                        } else if *val > 0 {
                            Some("+")
                        } else {
                            // val == 0
                            None
                        };
                        if let Some(sign) = sign {
                            tokens.extend(vec![
                                InstructionTextToken::new(
                                    BnString::new(sign),
                                    InstructionTextTokenContents::Text,
                                ),
                                InstructionTextToken::new(
                                    BnString::new(val.abs().to_string()),
                                    InstructionTextTokenContents::Integer(val.abs() as u64),
                                ),
                            ])
                        }
                    }
                    LdType::SPFromHL => {
                        tokens.extend(R16::SP.to_tokens(addr));
                        tokens.extend(separator.clone());
                        tokens.extend(R16::HL.to_tokens(addr));
                    }
                }
            }
            Add(src) | Adc(src) | Sub(src) | Sbc(src) | And(src) | Xor(src) | Or(src) | Cp(src) => {
                tokens.extend(spaces.clone());
                tokens.extend(R8::A.to_tokens(addr));
                tokens.extend(separator.clone());
                tokens.extend(src.to_tokens(addr));
            }

            IncR8(r8) | DecR8(r8) | Rlc(r8) | Rrc(r8) | Rl(r8) | Rr(r8) | Sla(r8) | Sra(r8)
            | Swap(r8) | Srl(r8) => {
                tokens.extend(spaces.clone());
                tokens.extend(r8.to_tokens(addr));
            }
            IncR16(r16) | DecR16(r16) => {
                tokens.extend(spaces.clone());
                tokens.extend(r16.to_tokens(addr));
            }

            AddHL(r16) => {
                tokens.extend(spaces.clone());
                tokens.extend(R16::HL.to_tokens(addr));
                tokens.extend(separator.clone());
                tokens.extend(r16.to_tokens(addr));
            }
            AddSP(val) => {
                tokens.extend(spaces.clone());
                tokens.extend(R16::SP.to_tokens(addr));
                tokens.extend(separator.clone());
                let sign = if *val < 0 {
                    Some("-")
                } else if *val > 0 {
                    Some("+")
                } else {
                    // val == 0
                    None
                };
                if let Some(sign) = sign {
                    tokens.push(InstructionTextToken::new(
                        BnString::new(sign),
                        InstructionTextTokenContents::Text,
                    ));
                }
                tokens.push(InstructionTextToken::new(
                    BnString::new(val.abs().to_string()),
                    InstructionTextTokenContents::Integer(val.abs() as u64),
                ));
            }

            Bit(bit, r8) | Res(bit, r8) | Set(bit, r8) => {
                tokens.extend(spaces.clone());
                tokens.extend(bit.to_tokens(addr));
                tokens.extend(separator.clone());
                tokens.extend(r8.to_tokens(addr));
            }

            Jr(cond, val) => {
                tokens.extend(spaces.clone());
                if let Some(cond) = cond {
                    tokens.extend(cond.to_tokens(addr));
                    tokens.extend(separator.clone());
                }
                let addr = (addr as u16)
                    .wrapping_add(self.length() as u16)
                    .wrapping_add_signed(*val as i16);
                tokens.push(InstructionTextToken::new(
                    BnString::new(format!("{addr:#06X}")),
                    InstructionTextTokenContents::PossibleAddress(addr as u64),
                ));
            }
            Jp(cond, val) | Call(cond, val) => {
                tokens.extend(spaces.clone());
                if let Some(cond) = cond {
                    tokens.extend(cond.to_tokens(addr));
                    tokens.extend(separator.clone());
                }
                tokens.push(InstructionTextToken::new(
                    BnString::new(format!("{val:#06X}")),
                    InstructionTextTokenContents::PossibleAddress(*val as u64),
                ))
            }
            Ret(Some(cond)) => {
                tokens.extend(spaces.clone());
                tokens.extend(cond.to_tokens(addr));
            }
            Rst(val) => {
                tokens.extend(spaces.clone());
                tokens.push(InstructionTextToken::new(
                    BnString::new(format!("{val:#06X}")),
                    InstructionTextTokenContents::PossibleAddress(*val as u64),
                ))
            }
            JpHL => {
                tokens.extend(spaces.clone());
                tokens.extend(R16::HL.to_tokens(addr));
            }

            Push(reg) | Pop(reg) => {
                tokens.extend(spaces.clone());
                tokens.extend(reg.to_tokens(addr));
            }

            Nop | Rlca | Rla | Rrca | Rra | Ret(None) | Reti | Scf | Ccf | Daa | Cpl | Stop
            | Halt | Di | Ei | Illegal => {}
        };

        tokens
    }
}

fn u16_arg(data: &[u8]) -> Option<u16> {
    let b1 = data.get(1)?;
    let b2 = data.get(2)?;
    Some(u16::from_le_bytes([*b1, *b2]))
}

fn u8_arg(data: &[u8]) -> Option<u8> {
    data.get(1).copied()
}

#[derive(Copy, Clone, Debug)]
pub enum LdType {
    R8(R8, R8),
    R8Imm(R8, u8),
    R16Imm(R16, u16),
    AFromInd(Indirect),
    IndFromA(Indirect),
    AFromIoReg(Io),
    IoRegFromA(Io),
    AFromMem(u16),
    MemFromA(u16),
    StoreSP(u16),
    HLFromSP(i8),
    SPFromHL,
}

#[derive(Copy, Clone, Primitive)]
pub enum R8 {
    A = 7,
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HLInd = 6,
}

impl fmt::Debug for R8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                R8::A => "A",
                R8::B => "B",
                R8::C => "C",
                R8::D => "D",
                R8::E => "E",
                R8::H => "H",
                R8::L => "L",
                R8::HLInd => "(HL)",
            }
        )
    }
}

impl ToTokens for R8 {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        match self {
            R8::A => vec![InstructionTextToken::new(
                BnString::new("A"),
                InstructionTextTokenContents::Register,
            )],
            R8::B => vec![InstructionTextToken::new(
                BnString::new("B"),
                InstructionTextTokenContents::Register,
            )],
            R8::C => vec![InstructionTextToken::new(
                BnString::new("C"),
                InstructionTextTokenContents::Register,
            )],
            R8::D => vec![InstructionTextToken::new(
                BnString::new("D"),
                InstructionTextTokenContents::Register,
            )],
            R8::E => vec![InstructionTextToken::new(
                BnString::new("E"),
                InstructionTextTokenContents::Register,
            )],
            R8::H => vec![InstructionTextToken::new(
                BnString::new("H"),
                InstructionTextTokenContents::Register,
            )],
            R8::L => vec![InstructionTextToken::new(
                BnString::new("L"),
                InstructionTextTokenContents::Register,
            )],
            R8::HLInd => vec![
                InstructionTextToken::new(
                    BnString::new("("),
                    InstructionTextTokenContents::BeginMemoryOperand,
                ),
                InstructionTextToken::new(
                    BnString::new("HL"),
                    InstructionTextTokenContents::Register,
                ),
                InstructionTextToken::new(
                    BnString::new(")"),
                    InstructionTextTokenContents::EndMemoryOperand,
                ),
            ],
        }
    }
}

#[derive(Copy, Clone, Debug, Primitive)]
pub enum R16 {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

impl ToTokens for R16 {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        vec![InstructionTextToken::new(
            BnString::new(format!("{self:?}")),
            InstructionTextTokenContents::Register,
        )]
    }
}

#[derive(Copy, Clone, Primitive)]
pub enum Indirect {
    BC = 0,
    DE = 1,
    HLInc = 2,
    HLDec = 3,
}

impl fmt::Debug for Indirect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({})",
            match self {
                Indirect::BC => "BE",
                Indirect::DE => "DE",
                Indirect::HLInc => "HL+",
                Indirect::HLDec => "HL-",
            }
        )
    }
}

impl ToTokens for Indirect {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        let mut tokens = match self {
            Indirect::BC => vec![InstructionTextToken::new(
                BnString::new("HL"),
                InstructionTextTokenContents::Register,
            )],
            Indirect::DE => vec![InstructionTextToken::new(
                BnString::new("DE"),
                InstructionTextTokenContents::Register,
            )],
            Indirect::HLInc => vec![
                InstructionTextToken::new(
                    BnString::new("HL"),
                    InstructionTextTokenContents::Register,
                ),
                InstructionTextToken::new(BnString::new("+"), InstructionTextTokenContents::Text),
            ],
            Indirect::HLDec => vec![
                InstructionTextToken::new(
                    BnString::new("HL"),
                    InstructionTextTokenContents::Register,
                ),
                InstructionTextToken::new(BnString::new("-"), InstructionTextTokenContents::Text),
            ],
        };
        tokens.insert(
            0,
            InstructionTextToken::new(
                BnString::new("("),
                InstructionTextTokenContents::BeginMemoryOperand,
            ),
        );
        tokens.push(InstructionTextToken::new(
            BnString::new(")"),
            InstructionTextTokenContents::EndMemoryOperand,
        ));
        tokens
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Io {
    C,
    Imm(u8),
}

impl ToTokens for Io {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        let mut tokens = match self {
            Io::C => vec![
                InstructionTextToken::new(
                    BnString::new("0xFF00"),
                    InstructionTextTokenContents::PossibleAddress(0xFF00),
                ),
                InstructionTextToken::new(BnString::new("+"), InstructionTextTokenContents::Text),
                InstructionTextToken::new(
                    BnString::new("C"),
                    InstructionTextTokenContents::Register,
                ),
            ],
            Io::Imm(val) => vec![InstructionTextToken::new(
                BnString::new(format!("{:#06X}", 0xFF00 + *val as u16)),
                InstructionTextTokenContents::PossibleAddress(0xFF00 + *val as u64),
            )],
        };

        tokens.insert(
            0,
            InstructionTextToken::new(
                BnString::new("("),
                InstructionTextTokenContents::BeginMemoryOperand,
            ),
        );
        tokens.push(InstructionTextToken::new(
            BnString::new(")"),
            InstructionTextTokenContents::EndMemoryOperand,
        ));
        tokens
    }
}

#[derive(Copy, Clone)]
pub enum AluSrc {
    R8(R8),
    Imm(u8),
}

impl fmt::Debug for AluSrc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AluSrc::R8(r8) => r8.fmt(f),
            AluSrc::Imm(val) => write!(f, "${val:02x}"),
        }
    }
}

impl ToTokens for AluSrc {
    fn to_tokens(&self, addr: u64) -> Vec<InstructionTextToken> {
        match self {
            AluSrc::R8(r8) => r8.to_tokens(addr),
            AluSrc::Imm(val) => vec![InstructionTextToken::new(
                BnString::new(format!("{val:#04x}")),
                InstructionTextTokenContents::Integer(*val as u64),
            )],
        }
    }
}

#[derive(Copy, Clone, Primitive)]
pub enum BitPos {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
}

impl fmt::Debug for BitPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (*self as u8).fmt(f)
    }
}

impl ToTokens for BitPos {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        vec![InstructionTextToken::new(
            BnString::new(format!("{self:?}")),
            InstructionTextTokenContents::Integer(*self as u64),
        )]
    }
}

#[derive(Copy, Clone, Debug, Primitive)]
pub enum BranchCond {
    NZ = 0,
    Z = 1,
    NC = 2,
    C = 3,
}

impl ToTokens for BranchCond {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        vec![InstructionTextToken::new(
            BnString::new(format!("{self:?}")),
            InstructionTextTokenContents::Text,
        )]
    }
}

#[derive(Copy, Clone, Debug, Primitive)]
pub enum PushPop {
    BC = 0,
    DE = 1,
    HL = 2,
    AF = 3,
}

impl ToTokens for PushPop {
    fn to_tokens(&self, _addr: u64) -> Vec<InstructionTextToken> {
        vec![InstructionTextToken::new(
            BnString::new(format!("{self:?}")),
            InstructionTextTokenContents::Register,
        )]
    }
}
