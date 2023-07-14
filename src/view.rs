use binaryninja::{
    architecture::{Architecture, ArchitectureExt, CoreArchitecture},
    binaryview::{BinaryView, BinaryViewBase, BinaryViewExt, Result as BinaryViewResult},
    custombinaryview::{
        BinaryViewType, BinaryViewTypeBase, CustomBinaryView, CustomBinaryViewType, CustomView,
        CustomViewBuilder,
    },
    rc::Ref,
    section::{Section, Semantics},
    segment::Segment,
    symbol::{Symbol, SymbolType},
    types::{
        MemberAccess, MemberScope, NamedTypeReference, NamedTypeReferenceClass, Structure, Type,
    },
    Endianness,
};
use log::debug;

const NINTENDO_LOGO: &[u8; 0x30] = b"\xCE\xED\x66\x66\xCC\x0D\x00\x0B\x03\x73\x00\x83\x00\x0C\x00\x0D\x00\x08\x11\x1F\x88\x89\x00\x0E\xDC\xCC\x6E\xE6\xDD\xDD\xD9\x99\xBB\xBB\x67\x63\x6E\x0E\xEC\xCC\xDD\xDC\x99\x9F\xBB\xB9\x33\x3E";
const HEADER_OFFSET: u64 = 0x100;
const HEADER_LEN: usize = 0x50;

pub struct GameBoyViewType {
    inner: BinaryViewType,
}

impl GameBoyViewType {
    pub fn new(view_type: BinaryViewType) -> Self {
        Self { inner: view_type }
    }
}

impl AsRef<BinaryViewType> for GameBoyViewType {
    fn as_ref(&self) -> &BinaryViewType {
        &self.inner
    }
}

impl BinaryViewTypeBase for GameBoyViewType {
    fn is_deprecated(&self) -> bool {
        false
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

impl CustomBinaryViewType for GameBoyViewType {
    fn create_custom_view<'builder>(
        &self,
        data: &BinaryView,
        builder: CustomViewBuilder<'builder, Self>,
    ) -> BinaryViewResult<CustomView<'builder>> {
        debug!("Creating GameBoyView from register GameBoyViewType");

        builder.create::<GameBoyView>(data, ())
    }
}

pub struct GameBoyView {
    inner: Ref<BinaryView>,
}

impl GameBoyView {
    fn new(view: &BinaryView) -> Self {
        Self {
            inner: view.to_owned(),
        }
    }

    fn add_segments_sections(&self) {
        self.add_segment(
            Segment::builder(0..0x8000)
                .parent_backing(0..0x8000)
                .is_auto(true)
                .readable(true)
                .executable(true),
        );
        self.add_segment(
            Segment::builder(0x8000..0x10000)
                .is_auto(true)
                .readable(true)
                .writable(true)
                .executable(true),
        );

        self.add_section(
            Section::builder("ISR", 0..0x100)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
        );
        self.add_section(
            Section::builder("EntryPoint", 0x100..0x104)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
        );
        self.add_section(
            Section::builder("HDR", 0x104..0x150)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyData),
        );
        self.add_section(
            Section::builder("ROM", 0x150..0x8000)
                .is_auto(true)
                .semantics(Semantics::ReadOnlyCode),
        );
        self.add_section(
            Section::builder("RAM", 0x8000..0x10000)
                .is_auto(true)
                .semantics(Semantics::ReadWriteData),
        );
    }

    fn apply_header_type(&self) -> BinaryViewResult<()> {
        let builder = Structure::builder();
        let members = [
            (&*Type::array(&*Type::char(), 0x30), "logo"),
            (&*Type::array(&*Type::char(), 0x10), "title"),
            // (&*Type::array(&*Type::char(), 0x4), "manufacturer"),
            // (&*Type::int(1, false), "cgb_flag"),
            (&*Type::array(&*Type::char(), 0x2), "new_licensee"),
            (&*Type::int(1, false), "sgb_flag"),
            (&*Type::int(1, false), "cartridge_type"),
            (&*Type::int(1, false), "rom_size"),
            (&*Type::int(1, false), "ram_size"),
            (&*Type::int(1, false), "destination"),
            (&*Type::int(1, false), "old_licensee"),
            (&*Type::int(1, false), "version"),
            (&*Type::int(1, false), "checksum"),
            (&*Type::int(2, false), "global_checksum"),
        ];
        for (ty, name) in members.into_iter() {
            builder.append(ty, name, MemberAccess::NoAccess, MemberScope::NoScope);
        }
        let header_type = Type::structure(builder.finalize().as_ref());
        let type_name = self.define_auto_type("GB_HEADER", "gb", &header_type);
        let named_type = Type::named_type(&NamedTypeReference::new(
            NamedTypeReferenceClass::StructNamedTypeClass,
            type_name,
        ));

        let symbol = Symbol::builder(SymbolType::Data, "gb_header", 0x104).create();
        self.define_auto_symbol_with_type(
            symbol.as_ref(),
            self.default_platform().unwrap().as_ref(),
            named_type.as_ref(),
        )?;
        Ok(())
    }

    fn init(&self) -> BinaryViewResult<()> {
        let arch = CoreArchitecture::by_name("gb").ok_or(())?;
        let platform = arch.standalone_platform().ok_or(())?;
        self.set_default_arch(&arch);
        self.set_default_platform(&platform);
        self.add_segments_sections();
        self.apply_header_type()?;
        self.add_entry_point(&platform, self.entry_point());
        Ok(())
    }
}

impl AsRef<BinaryView> for GameBoyView {
    fn as_ref(&self) -> &BinaryView {
        &self.inner
    }
}

impl BinaryViewBase for GameBoyView {
    fn address_size(&self) -> usize {
        self.default_arch().unwrap().address_size()
    }

    fn default_endianness(&self) -> Endianness {
        self.default_arch().unwrap().endianness()
    }

    fn entry_point(&self) -> u64 {
        0x100
    }
}

unsafe impl CustomBinaryView for GameBoyView {
    type Args = ();

    fn new(handle: &BinaryView, _args: &Self::Args) -> BinaryViewResult<Self> {
        Ok(Self::new(handle))
    }

    fn init(&self, _args: Self::Args) -> BinaryViewResult<()> {
        self.init()
    }
}
