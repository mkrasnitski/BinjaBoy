use crate::flag::{Flag, FlagClass, FlagGroup, FlagWrite};
use crate::instruction::{Instruction, ToTokens};
use binaryninja::{
    architecture::{
        self, Architecture, BranchInfo, CoreArchitecture, CustomArchitectureHandle, FlagCondition,
        ImplicitRegisterExtend, InstructionInfo, RegisterInfo,
    },
    disassembly::InstructionTextToken,
    llil::{LiftedExpr, Lifter},
    Endianness,
};
use enum_primitive_derive::Primitive;
use log::error;
use num_traits::FromPrimitive;
use std::borrow::Cow;

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
    type Register = Register;
    type RegisterInfo = Register;
    type Flag = Flag;
    type FlagWrite = FlagWrite;
    type FlagClass = FlagClass;
    type FlagGroup = FlagGroup;

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

    fn opcode_display_len(&self) -> usize {
        self.max_instr_len()
    }

    fn associated_arch_by_addr(&self, _: &mut u64) -> CoreArchitecture {
        self.handle
    }

    fn instruction_info(&self, data: &[u8], address: u64) -> Option<InstructionInfo> {
        let instr = Instruction::decode(data)?;
        let mut info = InstructionInfo::new(instr.length(), false);

        let addr = address as u16;
        let next_instr = addr.wrapping_add(instr.length() as u16);
        match instr {
            Instruction::Jr(cond, offset) => {
                if cond.is_some() {
                    info.add_branch(
                        BranchInfo::True(next_instr.wrapping_add_signed(offset as i16) as u64),
                        Some(self.handle),
                    );
                    info.add_branch(BranchInfo::False(next_instr as u64), Some(self.handle));
                } else {
                    info.add_branch(
                        BranchInfo::Unconditional(
                            next_instr.wrapping_add_signed(offset as i16) as u64
                        ),
                        Some(self.handle),
                    )
                }
            }
            Instruction::Jp(cond, addr) => {
                if cond.is_some() {
                    info.add_branch(BranchInfo::True(addr as u64), Some(self.handle));
                    info.add_branch(BranchInfo::False(next_instr as u64), Some(self.handle));
                } else {
                    info.add_branch(BranchInfo::Unconditional(addr as u64), Some(self.handle))
                }
            }
            Instruction::JpHL => info.add_branch(BranchInfo::Indirect, Some(self.handle)),
            Instruction::Call(_, addr) => {
                info.add_branch(BranchInfo::Call(addr as u64), Some(self.handle))
            }
            Instruction::Ret(Some(_)) => {} // conditional returns don't end the block
            Instruction::Ret(None) | Instruction::Reti => {
                info.add_branch(BranchInfo::FunctionReturn, Some(self.handle))
            }
            // unsure if this is correct - binja's z80 support doesn't do this
            Instruction::Rst(addr) => {
                info.add_branch(BranchInfo::Call(addr as u64), Some(self.handle))
            }
            Instruction::Stop => info.add_branch(BranchInfo::Exception, Some(self.handle)),
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
        _il: &mut Lifter<Self>,
    ) -> Option<(usize, bool)> {
        None
    }

    fn flags_required_for_flag_condition(
        &self,
        _condition: FlagCondition,
        _class: Option<Self::FlagClass>,
    ) -> Vec<Self::Flag> {
        vec![]
    }

    fn flag_group_llil<'a>(
        &self,
        _group: Self::FlagGroup,
        _il: &'a mut Lifter<Self>,
    ) -> Option<LiftedExpr<'a, Self>> {
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

    fn registers_global(&self) -> Vec<Self::Register> {
        vec![]
    }

    fn registers_system(&self) -> Vec<Self::Register> {
        vec![]
    }

    fn flags(&self) -> Vec<Self::Flag> {
        vec![Flag::Z, Flag::N, Flag::H, Flag::C]
    }

    fn flag_write_types(&self) -> Vec<Self::FlagWrite> {
        vec![FlagWrite::All, FlagWrite::Czn, FlagWrite::Zn]
    }

    fn flag_classes(&self) -> Vec<Self::FlagClass> {
        vec![]
    }

    fn flag_groups(&self) -> Vec<Self::FlagGroup> {
        vec![]
    }

    fn stack_pointer_reg(&self) -> Option<Self::Register> {
        Some(Register::SP)
    }

    fn link_reg(&self) -> Option<Self::Register> {
        None
    }

    fn register_from_id(&self, id: u32) -> Option<Self::Register> {
        let register = Register::from_u32(id);
        if register.is_none() {
            error!("invalid register id {}", id);
        }
        register
    }

    fn flag_from_id(&self, id: u32) -> Option<Self::Flag> {
        let flag = Flag::from_u32(id);
        if flag.is_none() {
            error!("invalid flag id {}", id);
        }
        flag
    }

    fn flag_write_from_id(&self, id: u32) -> Option<Self::FlagWrite> {
        let flag_write = FlagWrite::from_u32(id);
        if flag_write.is_none() {
            error!("invalid flag write id {}", id);
        }
        flag_write
    }

    fn flag_class_from_id(&self, _: u32) -> Option<Self::FlagClass> {
        None
    }

    fn flag_group_from_id(&self, _: u32) -> Option<Self::FlagGroup> {
        None
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

#[derive(Clone, Copy, Primitive)]
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

impl architecture::Register for Register {
    type InfoType = Self;

    fn name(&self) -> Cow<str> {
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

    fn id(&self) -> u32 {
        *self as u32
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
