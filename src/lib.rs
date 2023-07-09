use binaryninja::architecture::register_architecture;
use log::LevelFilter;

use arch::GameBoy;

mod arch;
mod flag;
mod instruction;

#[no_mangle]
pub extern "C" fn CorePluginInit() -> bool {
    binaryninja::logger::init(LevelFilter::Trace).expect("failed to setup logging");

    register_architecture("gb", GameBoy::new);
    true
}
