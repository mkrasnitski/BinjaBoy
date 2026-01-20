use crate::flag::{Flag, FlagClass, FlagGroup, FlagWrite};
use crate::instruction::{Instruction, ToTokens};
use binaryninja::{
    architecture::{
        self, Architecture, BranchKind, CoreArchitecture, CustomArchitectureHandle, FlagId,
        FlagWriteId, ImplicitRegisterExtend, InstructionInfo, RegisterId, RegisterInfo,
        UnusedIntrinsic, UnusedRegisterStack,
    },
    disassembly::InstructionTextToken,
    low_level_il::LowLevelILMutableFunction,
    Endianness,
};
use std::borrow::Cow;
use tracing::error;

pub struct GameBoy {
    handle: CoreArchitecture,
    custom_handle: CustomArchitectureHandle<Self>,
}

impl GameBoy {
    pub fn new(custom_handle: CustomArchitectureHandle<Self>, handle: CoreArchitecture) -> Self {
        Self {
            handle,
            custom_handle,
        }
    }
}

impl Architecture for GameBoy {
    type Handle = CustomArchitectureHandle<Self>;

    type RegisterInfo = Register;
    type Register = Register;
    type RegisterStackInfo = UnusedRegisterStack<Register>;
    type RegisterStack = UnusedRegisterStack<Register>;

    type Flag = Flag;
    type FlagWrite = FlagWrite;
    type FlagClass = FlagClass;
    type FlagGroup = FlagGroup;

    type Intrinsic = UnusedIntrinsic;

    fn endianness(&self) -> Endianness {
        Endianness::LittleEndian
    }

    fn address_size(&self) -> usize {
        2
    }

    fn default_integer_size(&self) -> usize {
        1
    }

    fn instruction_alignment(&self) -> usize {
        1
    }

    fn max_instr_len(&self) -> usize {
        3
    }

    fn instruction_info(&self, data: &[u8], address: u64) -> Option<InstructionInfo> {
        let instr = Instruction::decode(data)?;
        let mut info = InstructionInfo::new(instr.length(), 0);

        let addr = address as u16;
        let next_instr = addr.wrapping_add(instr.length() as u16);
        match instr {
            Instruction::Jr(cond, offset) => {
                if cond.is_some() {
                    info.add_branch(BranchKind::True(
                        next_instr.wrapping_add_signed(offset as i16) as u64,
                    ));
                    info.add_branch(BranchKind::False(next_instr as u64));
                } else {
                    info.add_branch(BranchKind::Unconditional(
                        next_instr.wrapping_add_signed(offset as i16) as u64,
                    ))
                }
            }
            Instruction::Jp(cond, addr) => {
                if cond.is_some() {
                    info.add_branch(BranchKind::True(addr as u64));
                    info.add_branch(BranchKind::False(next_instr as u64));
                } else {
                    info.add_branch(BranchKind::Unconditional(addr as u64))
                }
            }
            Instruction::JpHL => info.add_branch(BranchKind::Indirect),
            Instruction::Call(_, addr) => info.add_branch(BranchKind::Call(addr as u64)),
            Instruction::Ret(Some(_)) => {} // conditional returns don't end the block
            Instruction::Ret(None) | Instruction::Reti => {
                info.add_branch(BranchKind::FunctionReturn)
            }
            // unsure if this is correct - binja's z80 support doesn't do this
            Instruction::Rst(addr) => info.add_branch(BranchKind::Call(addr as u64)),
            Instruction::Stop => info.add_branch(BranchKind::Exception),
            _ => {}
        }
        Some(info)
    }

    fn instruction_text(
        &self,
        data: &[u8],
        address: u64,
    ) -> Option<(usize, Vec<InstructionTextToken>)> {
        let instr = Instruction::decode(data)?;
        Some((instr.length(), instr.to_tokens(address)))
    }

    fn instruction_llil(
        &self,
        _data: &[u8],
        _address: u64,
        _il: &LowLevelILMutableFunction,
    ) -> Option<(usize, bool)> {
        None
    }

    fn registers_all(&self) -> Vec<Self::Register> {
        use Register::*;
        vec![A, B, C, D, E, H, L, AF, BC, DE, HL, SP, PC, Flags]
    }

    fn registers_full_width(&self) -> Vec<Self::Register> {
        use Register::*;
        vec![AF, BC, DE, HL, SP, PC]
    }

    fn stack_pointer_reg(&self) -> Option<Self::Register> {
        Some(Register::SP)
    }

    fn flags(&self) -> Vec<Self::Flag> {
        vec![Flag::Z, Flag::N, Flag::H, Flag::C]
    }

    fn flag_from_id(&self, id: FlagId) -> Option<Self::Flag> {
        match id.0 {
            1 => Some(Flag::Z),
            2 => Some(Flag::N),
            3 => Some(Flag::H),
            4 => Some(Flag::C),
            _ => None,
        }
    }

    fn flag_write_types(&self) -> Vec<Self::FlagWrite> {
        vec![FlagWrite::All, FlagWrite::Czn, FlagWrite::Zn]
    }

    fn flag_write_from_id(&self, id: FlagWriteId) -> Option<Self::FlagWrite> {
        match id.0 {
            1 => Some(FlagWrite::All),
            2 => Some(FlagWrite::Czn),
            3 => Some(FlagWrite::Zn),
            _ => None,
        }
    }

    fn register_from_id(&self, id: RegisterId) -> Option<Self::Register> {
        match id.try_into() {
            Ok(flag) => Some(flag),
            Err(()) => {
                error!("invalid register id {id}");
                None
            }
        }
    }

    fn handle(&self) -> Self::Handle {
        self.custom_handle
    }
}

impl AsRef<CoreArchitecture> for GameBoy {
    fn as_ref(&self) -> &CoreArchitecture {
        &self.handle
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Register {
    A = 1,
    B = 2,
    C = 3,
    D = 4,
    E = 5,
    H = 6,
    L = 7,
    AF = 8,
    BC = 9,
    DE = 10,
    HL = 11,
    SP = 12,
    PC = 13,
    Flags = 14,
}

impl TryFrom<RegisterId> for Register {
    type Error = ();

    fn try_from(value: RegisterId) -> Result<Self, Self::Error> {
        match value.0 {
            1 => Ok(Self::A),
            2 => Ok(Self::B),
            3 => Ok(Self::C),
            4 => Ok(Self::D),
            5 => Ok(Self::E),
            6 => Ok(Self::H),
            7 => Ok(Self::L),
            8 => Ok(Self::AF),
            9 => Ok(Self::BC),
            10 => Ok(Self::DE),
            11 => Ok(Self::HL),
            12 => Ok(Self::SP),
            13 => Ok(Self::PC),
            14 => Ok(Self::Flags),
            _ => Err(()),
        }
    }
}

impl architecture::Register for Register {
    type InfoType = Self;

    fn name(&self) -> Cow<'_, str> {
        match self {
            Self::A => "A",
            Self::B => "B",
            Self::C => "C",
            Self::D => "D",
            Self::E => "E",
            Self::H => "H",
            Self::L => "L",
            Self::AF => "AF",
            Self::BC => "BC",
            Self::DE => "DE",
            Self::HL => "HL",
            Self::SP => "SP",
            Self::PC => "PC",
            Self::Flags => "Flags",
        }
        .into()
    }

    fn info(&self) -> Self::InfoType {
        *self
    }

    fn id(&self) -> RegisterId {
        RegisterId(*self as u32)
    }
}

impl RegisterInfo for Register {
    type RegType = Self;

    fn parent(&self) -> Option<Self::RegType> {
        match self {
            Self::A | Self::Flags => Some(Self::AF),
            Self::B | Self::C => Some(Self::BC),
            Self::D | Self::E => Some(Self::DE),
            Self::H | Self::L => Some(Self::HL),
            _ => None,
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::AF | Self::BC | Self::DE | Self::HL | Self::SP | Self::PC => 2,
            Self::A | Self::B | Self::C | Self::D | Self::E | Self::H | Self::L | Self::Flags => 1,
        }
    }

    fn offset(&self) -> usize {
        match self {
            Self::A | Self::B | Self::D | Self::H => 1,
            _ => 0,
        }
    }

    fn implicit_extend(&self) -> ImplicitRegisterExtend {
        ImplicitRegisterExtend::NoExtend
    }
}
