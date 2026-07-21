use binaryninja::{
    architecture::{ArchitectureExt, CoreArchitecture},
    binary_view::{BinaryView, BinaryViewBase, CustomBinaryView, CustomBinaryViewType},
    rc::Ref,
    section::{Section, SectionBuilder, Semantics},
    segment::{Segment, SegmentBuilder, SegmentFlags},
    symbol::{Symbol, SymbolType},
    types::{
        MemberAccess, MemberScope, NamedTypeReference, NamedTypeReferenceClass, Structure,
        StructureType, Type,
    },
    Endianness,
};

const NINTENDO_LOGO: &[u8; 0x30] = b"\xCE\xED\x66\x66\xCC\x0D\x00\x0B\x03\x73\x00\x83\x00\x0C\x00\x0D\x00\x08\x11\x1F\x88\x89\x00\x0E\xDC\xCC\x6E\xE6\xDD\xDD\xD9\x99\xBB\xBB\x67\x63\x6E\x0E\xEC\xCC\xDD\xDC\x99\x9F\xBB\xB9\x33\x3E";
const HEADER_OFFSET: u64 = 0x100;
const HEADER_LEN: usize = 0x50;

const INTERRUPT_HANDLERS: [(u16, &str); 13] = [
    (0x00, "isr_usr0"),
    (0x08, "isr_usr1"),
    (0x10, "isr_usr2"),
    (0x18, "isr_usr3"),
    (0x20, "isr_usr4"),
    (0x28, "isr_usr5"),
    (0x30, "isr_usr6"),
    (0x38, "isr_usr7"),
    (0x40, "isr_vblank"),
    (0x48, "isr_lcd"),
    (0x50, "isr_timer"),
    (0x58, "isr_serial"),
    (0x60, "isr_joypad"),
];

pub static IO_REGISTERS: [(u16, &str); 71] = [
    (0xFF00, "P1"),
    (0xFF01, "SB"),
    (0xFF02, "SC"),
    (0xFF04, "DIV"),
    (0xFF05, "TIMA"),
    (0xFF06, "TMA"),
    (0xFF07, "TAC"),
    (0xFF0F, "IF"),
    (0xFF10, "NR10"),
    (0xFF11, "NR11"),
    (0xFF12, "NR12"),
    (0xFF13, "NR13"),
    (0xFF14, "NR14"),
    (0xFF16, "NR21"),
    (0xFF17, "NR22"),
    (0xFF18, "NR23"),
    (0xFF19, "NR24"),
    (0xFF1A, "NR30"),
    (0xFF1B, "NR31"),
    (0xFF1C, "NR32"),
    (0xFF1D, "NR33"),
    (0xFF1E, "NR34"),
    (0xFF20, "NR41"),
    (0xFF21, "NR42"),
    (0xFF22, "NR43"),
    (0xFF23, "NR44"),
    (0xFF24, "NR50"),
    (0xFF25, "NR51"),
    (0xFF26, "NR52"),
    (0xFF30, "WAV0"),
    (0xFF31, "WAV1"),
    (0xFF32, "WAV2"),
    (0xFF33, "WAV3"),
    (0xFF34, "WAV4"),
    (0xFF35, "WAV5"),
    (0xFF36, "WAV6"),
    (0xFF37, "WAV7"),
    (0xFF38, "WAV8"),
    (0xFF39, "WAV9"),
    (0xFF3A, "WAVA"),
    (0xFF3B, "WAVB"),
    (0xFF3C, "WAVC"),
    (0xFF3D, "WAVD"),
    (0xFF3E, "WAVE"),
    (0xFF3F, "WAVF"),
    (0xFF40, "LCDC"),
    (0xFF41, "STAT"),
    (0xFF42, "SCY"),
    (0xFF43, "SCX"),
    (0xFF44, "LY"),
    (0xFF45, "LYC"),
    (0xFF46, "DMA"),
    (0xFF47, "BGP"),
    (0xFF48, "OBP0"),
    (0xFF49, "OBP1"),
    (0xFF4A, "WY"),
    (0xFF4B, "WX"),
    (0xFF4D, "KEY1"),
    (0xFF4F, "VBK"),
    (0xFF51, "HDMA1"),
    (0xFF52, "HDMA2"),
    (0xFF53, "HDMA3"),
    (0xFF54, "HDMA4"),
    (0xFF55, "HDMA5"),
    (0xFF56, "RP"),
    (0xFF68, "BCPS"),
    (0xFF69, "BCPD"),
    (0xFF6A, "OCPS"),
    (0xFF6B, "OCPD"),
    (0xFF70, "SVBK"),
    (0xFFFF, "IE"),
];

pub struct GameBoyViewType;

impl CustomBinaryViewType for GameBoyViewType {
    type CustomBinaryView = GameBoyView;
    const NAME: &'static str = "GameBoy";

    fn create_binary_view(&self, data: &BinaryView) -> Result<Self::CustomBinaryView, ()> {
        Ok(GameBoyView {
            inner: data.to_owned(),
        })
    }

    fn is_valid_for(&self, data: &BinaryView) -> bool {
        let mut header = Vec::new();
        data.read_into_vec(&mut header, HEADER_OFFSET, HEADER_LEN);
        if header.len() != HEADER_LEN {
            return false;
        }

        if &header[0x4..0x34] != NINTENDO_LOGO {
            return false;
        }

        true
    }
}

pub struct GameBoyView {
    inner: Ref<BinaryView>,
}

impl GameBoyView {
    fn segments() -> Vec<SegmentBuilder> {
        vec![
            Segment::builder(0..0x8000)
                .parent_backing(0..0x8000)
                .is_auto(true)
                .flags(SegmentFlags {
                    readable: true,
                    executable: true,
                    ..Default::default()
                }),
            Segment::builder(0x8000..0x10000)
                .is_auto(true)
                .flags(SegmentFlags {
                    readable: true,
                    writable: true,
                    executable: true,
                    ..Default::default()
                }),
        ]
    }

    fn sections() -> Vec<SectionBuilder> {
        vec![
            Section::builder("ISR".to_string(), 0..0x100)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
            Section::builder("EntryPoint".to_string(), 0x100..0x104)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
            Section::builder("HDR".to_string(), 0x104..0x150)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyData),
            Section::builder("ROM".to_string(), 0x150..0x8000)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
            Section::builder("RAM".to_string(), 0x8000..0x10000)
                .is_auto(true)
                .semantics(Semantics::ReadWriteData),
        ]
    }

    fn header_type() -> Ref<Type> {
        let new_title = Type::array(&Type::char(), 0x10);
        let old_title = Type::structure(
            Structure::builder()
                .append(
                    &Type::array(&Type::char(), 0xb),
                    "title",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::array(&Type::char(), 0x4),
                    "manufacturer",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "cgb_flag",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .finalize()
                .as_ref(),
        );
        let title = Type::structure(
            Structure::builder()
                .insert(
                    &new_title,
                    "title",
                    0,
                    false,
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .insert(
                    &old_title,
                    "title",
                    0,
                    false,
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .structure_type(StructureType::UnionStructureType)
                .finalize()
                .as_ref(),
        );
        let header_type = Type::structure(
            Structure::builder()
                .append(
                    &Type::array(&Type::char(), 0x30),
                    "logo",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &title,
                    "title",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::array(&Type::char(), 0x2),
                    "new_licensee",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "sgb_flag",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "cartridge_type",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "rom_size",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "ram_size",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "destination",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "old_licensee",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "version",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(1, false),
                    "checksum",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .append(
                    &Type::int(2, false),
                    "global_checksum",
                    MemberAccess::NoAccess,
                    MemberScope::NoScope,
                )
                .finalize()
                .as_ref(),
        );
        header_type
    }
}

impl AsRef<BinaryView> for GameBoyView {
    fn as_ref(&self) -> &BinaryView {
        &self.inner
    }
}

impl BinaryViewBase for GameBoyView {
    fn address_size(&self) -> usize {
        2
    }

    fn default_endianness(&self) -> Endianness {
        Endianness::LittleEndian
    }

    fn entry_point(&self) -> u64 {
        0x100
    }
}

impl CustomBinaryView for GameBoyView {
    fn initialize(&mut self, view: &BinaryView) -> bool {
        let Some(arch) = CoreArchitecture::by_name("gb") else {
            return false;
        };
        let Some(platform) = arch.standalone_platform() else {
            return false;
        };

        view.set_default_arch(&arch);
        view.set_default_platform(&platform);

        for segment in Self::segments() {
            view.add_segment(segment);
        }
        for section in Self::sections() {
            view.add_section(section);
        }

        view.add_entry_point(self.entry_point());

        let header_type = Self::header_type();
        let type_name = view.define_auto_type("GB_HEADER", "gb", &header_type);
        let named_type = Type::named_type(&NamedTypeReference::new(
            NamedTypeReferenceClass::StructNamedTypeClass,
            type_name,
        ));
        let symbol = Symbol::builder(SymbolType::Data, "gb_header", 0x104).create();
        let defined_symbol = view.define_auto_symbol_with_type(
            symbol.as_ref(),
            platform.as_ref(),
            named_type.as_ref(),
        );

        tracing::info!(
            "{:?}\n {:?}",
            defined_symbol,
            view.data_variable_at_address(0x104)
        );

        view.define_auto_symbol(
            Symbol::builder(SymbolType::Function, "_start", self.entry_point())
                .create()
                .as_ref(),
        );
        for (addr, name) in INTERRUPT_HANDLERS {
            view.define_auto_symbol(
                Symbol::builder(SymbolType::Function, name, addr as u64)
                    .create()
                    .as_ref(),
            );
        }
        for (addr, name) in IO_REGISTERS {
            view.define_auto_data_var(addr as u64, &Type::int(1, false));
            view.define_auto_symbol(
                Symbol::builder(SymbolType::Data, name, addr as u64)
                    .create()
                    .as_ref(),
            );
        }
        true
    }
}
