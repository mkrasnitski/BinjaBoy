use binaryninja::architecture::{self, FlagCondition, FlagRole};
use enum_primitive_derive::Primitive;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Primitive)]
pub enum Flag {
    Z = 1,
    N = 2,
    H = 3,
    C = 4,
}

impl architecture::Flag for Flag {
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        match self {
            Self::Z => "Z",
            Self::N => "N",
            Self::H => "H",
            Self::C => "C",
        }
        .into()
    }

    fn role(&self, _: Option<Self::FlagClass>) -> FlagRole {
        match self {
            Self::Z => FlagRole::ZeroFlagRole,
            Self::N => FlagRole::SpecialFlagRole,
            Self::H => FlagRole::HalfCarryFlagRole,
            Self::C => FlagRole::CarryFlagRole,
        }
    }

    fn id(&self) -> u32 {
        *self as u32
    }
}

#[derive(Clone, Copy, Primitive)]
pub enum FlagWrite {
    All = 1,
    Czn = 2,
    Zn = 3,
}

impl architecture::FlagWrite for FlagWrite {
    type FlagType = Flag;
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        match self {
            Self::All => "*",
            Self::Czn => "czn",
            Self::Zn => "zn",
        }
        .into()
    }

    fn class(&self) -> Option<Self::FlagClass> {
        None
    }

    fn id(&self) -> u32 {
        *self as u32
    }

    fn flags_written(&self) -> Vec<Self::FlagType> {
        match self {
            Self::All => vec![Flag::Z, Flag::N, Flag::H, Flag::C],
            Self::Czn => vec![Flag::C, Flag::Z, Flag::N],
            Self::Zn => vec![Flag::Z, Flag::N],
        }
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct FlagClass;

impl architecture::FlagClass for FlagClass {
    fn id(&self) -> u32 {
        unimplemented!()
    }

    fn name(&self) -> Cow<str> {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub struct FlagGroup;

impl architecture::FlagGroup for FlagGroup {
    type FlagType = Flag;
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        unimplemented!()
    }

    fn id(&self) -> u32 {
        unimplemented!()
    }

    fn flags_required(&self) -> Vec<Self::FlagType> {
        unimplemented!()
    }

    fn flag_conditions(&self) -> HashMap<Self::FlagClass, FlagCondition> {
        unimplemented!()
    }
}
