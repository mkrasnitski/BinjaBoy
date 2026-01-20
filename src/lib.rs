use binaryninja::{
    architecture::register_architecture,
    custom_binary_view::{register_view_type, BinaryViewTypeExt},
    Endianness,
};

use arch::GameBoy;

mod arch;
mod flag;
mod instruction;
mod view;

#[no_mangle]
pub extern "C" fn CorePluginInit() -> bool {
    binaryninja::tracing_init!("BinjaBoy");

    let arch = register_architecture("gb", GameBoy::new);
    let bv = register_view_type("GameBoy", "GameBoy", view::GameBoyViewType::new);
    bv.register_arch(0, Endianness::LittleEndian, arch);
    true
}
