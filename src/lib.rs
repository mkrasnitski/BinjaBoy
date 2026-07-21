use binaryninja::{
    architecture::register_architecture,
    binary_view::{register_binary_view_type, BinaryViewType, CustomBinaryViewType},
    Endianness,
};

use arch::GameBoy;
use view::GameBoyViewType;

mod arch;
mod flag;
mod instruction;
mod view;

#[no_mangle]
pub extern "C" fn CorePluginInit() -> bool {
    binaryninja::tracing_init!("BinjaBoy");

    let arch = register_architecture("gb", GameBoy::new);
    register_binary_view_type(GameBoyViewType);

    if let Some(bv) = BinaryViewType::by_name(GameBoyViewType::NAME) {
        bv.register_arch(0, Endianness::LittleEndian, arch);
    }
    true
}
