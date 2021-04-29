const Object = @This();

const std = @import("std");
const assert = std.debug.assert;
const dwarf = std.dwarf;
const fs = std.fs;
const io = std.io;
const log = std.log.scoped(.object);
const macho = std.macho;
const mem = std.mem;
const reloc = @import("reloc.zig");

const Allocator = mem.Allocator;
const Relocation = reloc.Relocation;
const parseName = @import("Zld.zig").parseName;

usingnamespace @import("commands.zig");

allocator: *Allocator,
arch: ?std.Target.Cpu.Arch = null,
header: ?macho.mach_header_64 = null,
file: ?fs.File = null,
file_offset: ?u32 = null,
name: ?[]u8 = null,

load_commands: std.ArrayListUnmanaged(LoadCommand) = .{},
sections: std.ArrayListUnmanaged(Section) = .{},

segment_cmd_index: ?u16 = null,
symtab_cmd_index: ?u16 = null,
dysymtab_cmd_index: ?u16 = null,
build_version_cmd_index: ?u16 = null,
data_in_code_cmd_index: ?u16 = null,

text_section_index: ?u16 = null,
mod_init_func_section_index: ?u16 = null,

dwarf_debug_info_index: ?u16 = null,
dwarf_debug_abbrev_index: ?u16 = null,
dwarf_debug_str_index: ?u16 = null,
dwarf_debug_line_index: ?u16 = null,
dwarf_debug_ranges_index: ?u16 = null,

symtab: std.ArrayListUnmanaged(Symbol) = .{},
strtab: std.ArrayListUnmanaged(u8) = .{},

dice: std.ArrayListUnmanaged(macho.data_in_code_entry) = .{},

pub const Section = struct {
    inner: macho.section_64,
    code: []u8,
    relocs: []macho.relocation_info,

    fn deinit(self: *Section, allocator: *Allocator) void {
        allocator.free(self.code);
        allocator.free(self.relocs);
    }
};

pub const Symbol = struct {
    tag: Tag,
    linkage: ?Linkage = null,
    address: ?u64 = null,
    section: ?u8 = null,
    inner: macho.nlist_64,

    pub const Tag = enum {
        defined,
        undef,
    };

    pub const Linkage = enum {
        translation_unit,
        linkage_unit,
        global,
    };

    fn isStab(sym: macho.nlist_64) bool {
        return (macho.N_STAB & sym.n_type) != 0;
    }

    fn isPrivateExt(sym: macho.nlist_64) bool {
        return (macho.N_PEXT & sym.n_type) != 0;
    }

    fn isExt(sym: macho.nlist_64) bool {
        return (macho.N_EXT & sym.n_type) != 0;
    }

    fn isSect(sym: macho.nlist_64) bool {
        const type_ = macho.N_TYPE & sym.n_type;
        return type_ == macho.N_SECT;
    }

    fn isUndef(sym: macho.nlist_64) bool {
        const type_ = macho.N_TYPE & sym.n_type;
        return type_ == macho.N_UNDF;
    }

    fn isWeakDef(sym: macho.nlist_64) bool {
        return (sym.n_desc & macho.N_WEAK_DEF) != 0;
    }

    fn isWeakRef(sym: macho.nlist_64) bool {
        return (sym.n_desc & macho.N_WEAK_REF) != 0;
    }
};

pub fn init(allocator: *Allocator) Object {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Object) void {
    for (self.load_commands.items) |*lc| {
        lc.deinit(self.allocator);
    }
    self.load_commands.deinit(self.allocator);

    for (self.sections.items) |*sect| {
        sect.deinit(self.allocator);
    }
    self.sections.deinit(self.allocator);

    self.symtab.deinit(self.allocator);
    self.strtab.deinit(self.allocator);
    self.dice.deinit(self.allocator);

    if (self.name) |n| {
        self.allocator.free(n);
    }
}

pub fn closeFile(self: Object) void {
    if (self.file) |f| {
        f.close();
    }
}

pub fn parse(self: *Object) !void {
    var reader = self.file.?.reader();
    if (self.file_offset) |offset| {
        try reader.context.seekTo(offset);
    }

    self.header = try reader.readStruct(macho.mach_header_64);

    if (self.header.?.filetype != macho.MH_OBJECT) {
        log.err("invalid filetype: expected 0x{x}, found 0x{x}", .{ macho.MH_OBJECT, self.header.?.filetype });
        return error.MalformedObject;
    }
    if (self.header.?.flags & macho.MH_SUBSECTIONS_VIA_SYMBOLS == 0) {
        log.err(
            "only objects compiled dead code stripping with flag MH_SUBSECTIONS_VIA_SYMBOLS are supported",
            .{},
        );
        return error.UnsupportedObjectType;
    }

    const this_arch: std.Target.Cpu.Arch = switch (self.header.?.cputype) {
        macho.CPU_TYPE_ARM64 => .aarch64,
        macho.CPU_TYPE_X86_64 => .x86_64,
        else => |value| {
            log.err("unsupported cpu architecture 0x{x}", .{value});
            return error.UnsupportedCpuArchitecture;
        },
    };
    if (this_arch != self.arch.?) {
        log.err("mismatched cpu architecture: expected {s}, found {s}", .{ self.arch.?, this_arch });
        return error.MismatchedCpuArchitecture;
    }

    try self.readLoadCommands(reader);

    if (self.symtab_cmd_index == null) return;

    try self.parseSymtab();
    try self.parseSections();
    try self.parseDataInCode();
}

fn readLoadCommands(self: *Object, reader: anytype) !void {
    const offset = self.file_offset orelse 0;
    try self.load_commands.ensureCapacity(self.allocator, self.header.?.ncmds);

    var i: u16 = 0;
    while (i < self.header.?.ncmds) : (i += 1) {
        var cmd = try LoadCommand.read(self.allocator, reader);
        switch (cmd.cmd()) {
            macho.LC_SEGMENT_64 => {
                self.segment_cmd_index = i;
                var seg = cmd.Segment;
                for (seg.sections.items) |*sect, j| {
                    const index = @intCast(u16, j);
                    const segname = parseName(&sect.segname);
                    const sectname = parseName(&sect.sectname);
                    if (mem.eql(u8, segname, "__DWARF")) {
                        if (mem.eql(u8, sectname, "__debug_info")) {
                            self.dwarf_debug_info_index = index;
                        } else if (mem.eql(u8, sectname, "__debug_abbrev")) {
                            self.dwarf_debug_abbrev_index = index;
                        } else if (mem.eql(u8, sectname, "__debug_str")) {
                            self.dwarf_debug_str_index = index;
                        } else if (mem.eql(u8, sectname, "__debug_line")) {
                            self.dwarf_debug_line_index = index;
                        } else if (mem.eql(u8, sectname, "__debug_ranges")) {
                            self.dwarf_debug_ranges_index = index;
                        }
                    } else if (mem.eql(u8, segname, "__TEXT")) {
                        if (mem.eql(u8, sectname, "__text")) {
                            self.text_section_index = index;
                        }
                    } else if (mem.eql(u8, segname, "__DATA")) {
                        if (mem.eql(u8, sectname, "__mod_init_func")) {
                            self.mod_init_func_section_index = index;
                        }
                    }

                    sect.offset += offset;
                    if (sect.reloff > 0) {
                        sect.reloff += offset;
                    }
                }

                seg.inner.fileoff += offset;
            },
            macho.LC_SYMTAB => {
                self.symtab_cmd_index = i;
                cmd.Symtab.symoff += offset;
                cmd.Symtab.stroff += offset;
            },
            macho.LC_DYSYMTAB => {
                self.dysymtab_cmd_index = i;
            },
            macho.LC_BUILD_VERSION => {
                self.build_version_cmd_index = i;
            },
            macho.LC_DATA_IN_CODE => {
                self.data_in_code_cmd_index = i;
                cmd.LinkeditData.dataoff += offset;
            },
            else => {
                log.debug("Unknown load command detected: 0x{x}.", .{cmd.cmd()});
            },
        }
        self.load_commands.appendAssumeCapacity(cmd);
    }
}

fn parseSections(self: *Object) !void {
    log.warn("parsing sections in {s}", .{self.name.?});

    const seg = self.load_commands.items[self.segment_cmd_index.?].Segment;
    try self.sections.ensureCapacity(self.allocator, seg.sections.items.len);

    for (seg.sections.items) |sect| {
        log.warn("parsing section '{s},{s}'", .{ parseName(&sect.segname), parseName(&sect.sectname) });

        // Read sections' code
        var code = try self.allocator.alloc(u8, sect.size);
        _ = try self.file.?.preadAll(code, sect.offset);

        // Read relocations
        var relocs = try self.allocator.alloc(u8, @sizeOf(macho.relocation_info) * sect.nreloc);
        _ = try self.file.?.preadAll(relocs, sect.reloff);

        self.sections.appendAssumeCapacity(.{
            .inner = sect,
            .code = code,
            .relocs = mem.bytesAsSlice(macho.relocation_info, relocs),
        });
    }
}

fn parseSymtab(self: *Object) !void {
    const symtab_cmd = self.load_commands.items[self.symtab_cmd_index.?].Symtab;

    var symtab = try self.allocator.alloc(u8, @sizeOf(macho.nlist_64) * symtab_cmd.nsyms);
    defer self.allocator.free(symtab);

    _ = try self.file.?.preadAll(symtab, symtab_cmd.symoff);
    const nlists = @alignCast(@alignOf(macho.nlist_64), mem.bytesAsSlice(macho.nlist_64, symtab));

    var strtab = try self.allocator.alloc(u8, symtab_cmd.strsize);
    defer self.allocator.free(strtab);

    _ = try self.file.?.preadAll(strtab, symtab_cmd.stroff);
    try self.strtab.appendSlice(self.allocator, strtab);
    try self.symtab.ensureCapacity(self.allocator, nlists.len);

    for (nlists) |nlist| {
        const tag: Symbol.Tag = if (Symbol.isSect(nlist)) .defined else .undef;
        const linkage: ?Symbol.Linkage = linkage: {
            if (Symbol.isUndef(nlist)) break :linkage null;
            if (Symbol.isExt(nlist)) {
                if (Symbol.isWeakDef(nlist) or Symbol.isPrivateExt(nlist)) break :linkage .linkage_unit;
                break :linkage .global;
            }
            break :linkage .translation_unit;
        };
        const address: ?u64 = if (Symbol.isUndef(nlist)) null else nlist.n_value;
        const section: ?u8 = if (Symbol.isUndef(nlist)) null else nlist.n_sect - 1;

        self.symtab.appendAssumeCapacity(.{
            .tag = tag,
            .inner = nlist,
            .linkage = linkage,
            .address = address,
            .section = section,
        });
    }
}

fn parseDataInCode(self: *Object) !void {
    const index = self.data_in_code_cmd_index orelse return;
    const data_in_code = self.load_commands.items[index].LinkeditData;

    var buffer = try self.allocator.alloc(u8, data_in_code.datasize);
    defer self.allocator.free(buffer);

    _ = try self.file.?.preadAll(buffer, data_in_code.dataoff);

    var stream = io.fixedBufferStream(buffer);
    var reader = stream.reader();
    while (true) {
        const dice = reader.readStruct(macho.data_in_code_entry) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        try self.dice.append(self.allocator, dice);
    }
}

pub fn getString(self: *const Object, str_off: u32) []const u8 {
    assert(str_off < self.strtab.items.len);
    return mem.spanZ(@ptrCast([*:0]const u8, self.strtab.items.ptr + str_off));
}

pub fn printSymtab(self: Object) void {
    log.warn("{s}: symtab", .{self.name.?});

    for (self.symtab.items) |sym| {
        const name = self.getString(sym.inner.n_strx);

        switch (sym.tag) {
            .undef => log.warn("    | {s} => {{ {s} }}", .{ name, sym.tag }),
            .defined => log.warn("    | {s} => {{ {s}, {s}, 0x{x}, {} }}", .{
                name,
                sym.tag,
                sym.linkage.?,
                sym.address.?,
                sym.section.?,
            }),
        }
    }
}
