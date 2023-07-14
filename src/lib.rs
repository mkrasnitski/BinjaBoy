use binaryninja::{
    architecture::register_architecture,
    custombinaryview::{register_view_type, BinaryViewTypeExt},
    Endianness,
};
use log::LevelFilter;

use arch::GameBoy;

mod arch;
mod flag;
mod instruction;
mod view;

#[no_mangle]
pub extern "C" fn CorePluginInit() -> bool {
    binaryninja::logger::init(LevelFilter::Trace).expect("failed to setup logging");

    let arch = register_architecture("gb", GameBoy::new);
    let bv = register_view_type("GameBoy", "GameBoy", view::GameBoyViewType::new);
    bv.register_arch(0, Endianness::LittleEndian, arch);
    true
}
