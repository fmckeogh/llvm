//===-- MCObjectFileInfo.cpp - Object File Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/BinaryFormat/COFF.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSection.h"
#include "llvm/MC/MCSectionCOFF.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/MC/MCSectionOMF.h"
#include "llvm/MC/MCSectionWasm.h"

using namespace llvm;

static bool useCompactUnwind(const Triple &T) {
  // Only on darwin.
  if (!T.isOSDarwin())
    return false;

  // aarch64 always has it.
  if (T.getArch() == Triple::aarch64)
    return true;

  // armv7k always has it.
  if (T.isWatchABI())
    return true;

  // Use it on newer version of OS X.
  if (T.isMacOSX() && !T.isMacOSXVersionLT(10, 6))
    return true;

  // And the iOS simulator.
  if (T.isiOS() &&
      (T.getArch() == Triple::x86_64 || T.getArch() == Triple::x86))
    return true;

  return false;
}

void MCObjectFileInfo::initMachOMCObjectFileInfo(const Triple &T) {
  // MachO
  SupportsWeakOmittedEHFrame = false;

  EHFrameSection = Ctx->getMachOSection(
      "__TEXT", "__eh_frame",
      MachO::S_COALESCED | MachO::S_ATTR_NO_TOC |
          MachO::S_ATTR_STRIP_STATIC_SYMS | MachO::S_ATTR_LIVE_SUPPORT,
      SectionKind::getReadOnly());

  if (T.isOSDarwin() && T.getArch() == Triple::aarch64)
    SupportsCompactUnwindWithoutEHFrame = true;

  if (T.isWatchABI())
    OmitDwarfIfHaveCompactUnwind = true;

  PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel
    | dwarf::DW_EH_PE_sdata4;
  LSDAEncoding = FDECFIEncoding = dwarf::DW_EH_PE_pcrel;
  TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
    dwarf::DW_EH_PE_sdata4;

  // .comm doesn't support alignment before Leopard.
  if (T.isMacOSX() && T.isMacOSXVersionLT(10, 5))
    CommDirectiveSupportsAlignment = false;

  TextSection // .text
    = Ctx->getMachOSection("__TEXT", "__text",
                           MachO::S_ATTR_PURE_INSTRUCTIONS,
                           SectionKind::getText());
  DataSection // .data
      = Ctx->getMachOSection("__DATA", "__data", 0, SectionKind::getData());

  // BSSSection might not be expected initialized on msvc.
  BSSSection = nullptr;

  TLSDataSection // .tdata
      = Ctx->getMachOSection("__DATA", "__thread_data",
                             MachO::S_THREAD_LOCAL_REGULAR,
                             SectionKind::getData());
  TLSBSSSection // .tbss
    = Ctx->getMachOSection("__DATA", "__thread_bss",
                           MachO::S_THREAD_LOCAL_ZEROFILL,
                           SectionKind::getThreadBSS());

  // TODO: Verify datarel below.
  TLSTLVSection // .tlv
      = Ctx->getMachOSection("__DATA", "__thread_vars",
                             MachO::S_THREAD_LOCAL_VARIABLES,
                             SectionKind::getData());

  TLSThreadInitSection = Ctx->getMachOSection(
      "__DATA", "__thread_init", MachO::S_THREAD_LOCAL_INIT_FUNCTION_POINTERS,
      SectionKind::getData());

  CStringSection // .cstring
    = Ctx->getMachOSection("__TEXT", "__cstring",
                           MachO::S_CSTRING_LITERALS,
                           SectionKind::getMergeable1ByteCString());
  UStringSection
    = Ctx->getMachOSection("__TEXT","__ustring", 0,
                           SectionKind::getMergeable2ByteCString());
  FourByteConstantSection // .literal4
    = Ctx->getMachOSection("__TEXT", "__literal4",
                           MachO::S_4BYTE_LITERALS,
                           SectionKind::getMergeableConst4());
  EightByteConstantSection // .literal8
    = Ctx->getMachOSection("__TEXT", "__literal8",
                           MachO::S_8BYTE_LITERALS,
                           SectionKind::getMergeableConst8());

  SixteenByteConstantSection // .literal16
      = Ctx->getMachOSection("__TEXT", "__literal16",
                             MachO::S_16BYTE_LITERALS,
                             SectionKind::getMergeableConst16());

  ReadOnlySection  // .const
    = Ctx->getMachOSection("__TEXT", "__const", 0,
                           SectionKind::getReadOnly());

  // If the target is not powerpc, map the coal sections to the non-coal
  // sections.
  //
  // "__TEXT/__textcoal_nt" => section "__TEXT/__text"
  // "__TEXT/__const_coal"  => section "__TEXT/__const"
  // "__DATA/__datacoal_nt" => section "__DATA/__data"
  Triple::ArchType ArchTy = T.getArch();

  if (ArchTy == Triple::ppc || ArchTy == Triple::ppc64) {
    TextCoalSection
      = Ctx->getMachOSection("__TEXT", "__textcoal_nt",
                             MachO::S_COALESCED |
                             MachO::S_ATTR_PURE_INSTRUCTIONS,
                             SectionKind::getText());
    ConstTextCoalSection
      = Ctx->getMachOSection("__TEXT", "__const_coal",
                             MachO::S_COALESCED,
                             SectionKind::getReadOnly());
    DataCoalSection = Ctx->getMachOSection(
        "__DATA", "__datacoal_nt", MachO::S_COALESCED, SectionKind::getData());
  } else {
    TextCoalSection = TextSection;
    ConstTextCoalSection = ReadOnlySection;
    DataCoalSection = DataSection;
  }

  ConstDataSection  // .const_data
    = Ctx->getMachOSection("__DATA", "__const", 0,
                           SectionKind::getReadOnlyWithRel());
  DataCommonSection
    = Ctx->getMachOSection("__DATA","__common",
                           MachO::S_ZEROFILL,
                           SectionKind::getBSS());
  DataBSSSection
    = Ctx->getMachOSection("__DATA","__bss", MachO::S_ZEROFILL,
                           SectionKind::getBSS());


  LazySymbolPointerSection
    = Ctx->getMachOSection("__DATA", "__la_symbol_ptr",
                           MachO::S_LAZY_SYMBOL_POINTERS,
                           SectionKind::getMetadata());
  NonLazySymbolPointerSection
    = Ctx->getMachOSection("__DATA", "__nl_symbol_ptr",
                           MachO::S_NON_LAZY_SYMBOL_POINTERS,
                           SectionKind::getMetadata());

  ThreadLocalPointerSection
    = Ctx->getMachOSection("__DATA", "__thread_ptr",
                           MachO::S_THREAD_LOCAL_VARIABLE_POINTERS,
                           SectionKind::getMetadata());

  // Exception Handling.
  LSDASection = Ctx->getMachOSection("__TEXT", "__gcc_except_tab", 0,
                                     SectionKind::getReadOnlyWithRel());

  COFFDebugSymbolsSection = nullptr;
  COFFDebugTypesSection = nullptr;

  if (useCompactUnwind(T)) {
    CompactUnwindSection =
        Ctx->getMachOSection("__LD", "__compact_unwind", MachO::S_ATTR_DEBUG,
                             SectionKind::getReadOnly());

    if (T.getArch() == Triple::x86_64 || T.getArch() == Triple::x86)
      CompactUnwindDwarfEHFrameOnly = 0x04000000;  // UNWIND_X86_64_MODE_DWARF
    else if (T.getArch() == Triple::aarch64)
      CompactUnwindDwarfEHFrameOnly = 0x03000000;  // UNWIND_ARM64_MODE_DWARF
    else if (T.getArch() == Triple::arm || T.getArch() == Triple::thumb)
      CompactUnwindDwarfEHFrameOnly = 0x04000000;  // UNWIND_ARM_MODE_DWARF
  }

  // Debug Information.
  DwarfAccelNamesSection =
      Ctx->getMachOSection("__DWARF", "__apple_names", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "names_begin");
  DwarfAccelObjCSection =
      Ctx->getMachOSection("__DWARF", "__apple_objc", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "objc_begin");
  // 16 character section limit...
  DwarfAccelNamespaceSection =
      Ctx->getMachOSection("__DWARF", "__apple_namespac", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "namespac_begin");
  DwarfAccelTypesSection =
      Ctx->getMachOSection("__DWARF", "__apple_types", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "types_begin");

  DwarfSwiftASTSection =
      Ctx->getMachOSection("__DWARF", "__swift_ast", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());

  DwarfAbbrevSection =
      Ctx->getMachOSection("__DWARF", "__debug_abbrev", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "section_abbrev");
  DwarfInfoSection =
      Ctx->getMachOSection("__DWARF", "__debug_info", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "section_info");
  DwarfLineSection =
      Ctx->getMachOSection("__DWARF", "__debug_line", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "section_line");
  DwarfFrameSection =
      Ctx->getMachOSection("__DWARF", "__debug_frame", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfPubNamesSection =
      Ctx->getMachOSection("__DWARF", "__debug_pubnames", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfPubTypesSection =
      Ctx->getMachOSection("__DWARF", "__debug_pubtypes", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfGnuPubNamesSection =
      Ctx->getMachOSection("__DWARF", "__debug_gnu_pubn", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfGnuPubTypesSection =
      Ctx->getMachOSection("__DWARF", "__debug_gnu_pubt", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfStrSection =
      Ctx->getMachOSection("__DWARF", "__debug_str", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "info_string");
  DwarfStrOffSection =
      Ctx->getMachOSection("__DWARF", "__debug_str_offs", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "section_str_off");
  DwarfLocSection =
      Ctx->getMachOSection("__DWARF", "__debug_loc", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "section_debug_loc");
  DwarfARangesSection =
      Ctx->getMachOSection("__DWARF", "__debug_aranges", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfRangesSection =
      Ctx->getMachOSection("__DWARF", "__debug_ranges", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "debug_range");
  DwarfMacinfoSection =
      Ctx->getMachOSection("__DWARF", "__debug_macinfo", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata(), "debug_macinfo");
  DwarfDebugInlineSection =
      Ctx->getMachOSection("__DWARF", "__debug_inlined", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfCUIndexSection =
      Ctx->getMachOSection("__DWARF", "__debug_cu_index", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  DwarfTUIndexSection =
      Ctx->getMachOSection("__DWARF", "__debug_tu_index", MachO::S_ATTR_DEBUG,
                           SectionKind::getMetadata());
  StackMapSection = Ctx->getMachOSection("__LLVM_STACKMAPS", "__llvm_stackmaps",
                                         0, SectionKind::getMetadata());

  FaultMapSection = Ctx->getMachOSection("__LLVM_FAULTMAPS", "__llvm_faultmaps",
                                         0, SectionKind::getMetadata());

  TLSExtraDataSection = TLSTLVSection;
}

void MCObjectFileInfo::initELFMCObjectFileInfo(const Triple &T, bool Large) {
  switch (T.getArch()) {
  case Triple::mips:
  case Triple::mipsel:
    FDECFIEncoding = dwarf::DW_EH_PE_sdata4;
    break;
  case Triple::mips64:
  case Triple::mips64el:
    FDECFIEncoding = dwarf::DW_EH_PE_sdata8;
    break;
  case Triple::x86_64:
    FDECFIEncoding = dwarf::DW_EH_PE_pcrel |
                     (Large ? dwarf::DW_EH_PE_sdata8 : dwarf::DW_EH_PE_sdata4);
    break;
  case Triple::bpfel:
  case Triple::bpfeb:
    FDECFIEncoding = dwarf::DW_EH_PE_sdata8;
    break;
  default:
    FDECFIEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
    break;
  }

  switch (T.getArch()) {
  case Triple::arm:
  case Triple::armeb:
  case Triple::thumb:
  case Triple::thumbeb:
    if (Ctx->getAsmInfo()->getExceptionHandlingType() == ExceptionHandling::ARM)
      break;
    // Fallthrough if not using EHABI
    LLVM_FALLTHROUGH;
  case Triple::ppc:
  case Triple::x86:
    PersonalityEncoding = PositionIndependent
                              ? dwarf::DW_EH_PE_indirect |
                                    dwarf::DW_EH_PE_pcrel |
                                    dwarf::DW_EH_PE_sdata4
                              : dwarf::DW_EH_PE_absptr;
    LSDAEncoding = PositionIndependent
                       ? dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4
                       : dwarf::DW_EH_PE_absptr;
    TTypeEncoding = PositionIndependent
                        ? dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
                              dwarf::DW_EH_PE_sdata4
                        : dwarf::DW_EH_PE_absptr;
    break;
  case Triple::x86_64:
    if (PositionIndependent) {
      PersonalityEncoding =
          dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
          (Large ? dwarf::DW_EH_PE_sdata8 : dwarf::DW_EH_PE_sdata4);
      LSDAEncoding = dwarf::DW_EH_PE_pcrel |
                     (Large ? dwarf::DW_EH_PE_sdata8 : dwarf::DW_EH_PE_sdata4);
      TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
                      (Large ? dwarf::DW_EH_PE_sdata8 : dwarf::DW_EH_PE_sdata4);
    } else {
      PersonalityEncoding =
          Large ? dwarf::DW_EH_PE_absptr : dwarf::DW_EH_PE_udata4;
      LSDAEncoding = Large ? dwarf::DW_EH_PE_absptr : dwarf::DW_EH_PE_udata4;
      TTypeEncoding = Large ? dwarf::DW_EH_PE_absptr : dwarf::DW_EH_PE_udata4;
    }
    break;
  case Triple::hexagon:
    PersonalityEncoding = dwarf::DW_EH_PE_absptr;
    LSDAEncoding = dwarf::DW_EH_PE_absptr;
    FDECFIEncoding = dwarf::DW_EH_PE_absptr;
    TTypeEncoding = dwarf::DW_EH_PE_absptr;
    if (PositionIndependent) {
      PersonalityEncoding |= dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel;
      LSDAEncoding |= dwarf::DW_EH_PE_pcrel;
      FDECFIEncoding |= dwarf::DW_EH_PE_pcrel;
      TTypeEncoding |= dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel;
    }
    break;
  case Triple::aarch64:
  case Triple::aarch64_be:
    // The small model guarantees static code/data size < 4GB, but not where it
    // will be in memory. Most of these could end up >2GB away so even a signed
    // pc-relative 32-bit address is insufficient, theoretically.
    if (PositionIndependent) {
      PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata8;
      LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata8;
      TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata8;
    } else {
      PersonalityEncoding = dwarf::DW_EH_PE_absptr;
      LSDAEncoding = dwarf::DW_EH_PE_absptr;
      TTypeEncoding = dwarf::DW_EH_PE_absptr;
    }
    break;
  case Triple::lanai:
    LSDAEncoding = dwarf::DW_EH_PE_absptr;
    PersonalityEncoding = dwarf::DW_EH_PE_absptr;
    TTypeEncoding = dwarf::DW_EH_PE_absptr;
    break;
  case Triple::mips:
  case Triple::mipsel:
  case Triple::mips64:
  case Triple::mips64el:
    // MIPS uses indirect pointer to refer personality functions and types, so
    // that the eh_frame section can be read-only. DW.ref.personality will be
    // generated for relocation.
    PersonalityEncoding = dwarf::DW_EH_PE_indirect;
    // FIXME: The N64 ABI probably ought to use DW_EH_PE_sdata8 but we can't
    //        identify N64 from just a triple.
    TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
                    dwarf::DW_EH_PE_sdata4;
    // We don't support PC-relative LSDA references in GAS so we use the default
    // DW_EH_PE_absptr for those.

    // FreeBSD must be explicit about the data size and using pcrel since it's
    // assembler/linker won't do the automatic conversion that the Linux tools
    // do.
    if (T.isOSFreeBSD()) {
      PersonalityEncoding |= dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
      LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
    }
    break;
  case Triple::ppc64:
  case Triple::ppc64le:
    PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
      dwarf::DW_EH_PE_udata8;
    LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_udata8;
    TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
      dwarf::DW_EH_PE_udata8;
    break;
  case Triple::sparcel:
  case Triple::sparc:
    if (PositionIndependent) {
      LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
      PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
      TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
    } else {
      LSDAEncoding = dwarf::DW_EH_PE_absptr;
      PersonalityEncoding = dwarf::DW_EH_PE_absptr;
      TTypeEncoding = dwarf::DW_EH_PE_absptr;
    }
    break;
  case Triple::sparcv9:
    LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
    if (PositionIndependent) {
      PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
      TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
    } else {
      PersonalityEncoding = dwarf::DW_EH_PE_absptr;
      TTypeEncoding = dwarf::DW_EH_PE_absptr;
    }
    break;
  case Triple::systemz:
    // All currently-defined code models guarantee that 4-byte PC-relative
    // values will be in range.
    if (PositionIndependent) {
      PersonalityEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
      LSDAEncoding = dwarf::DW_EH_PE_pcrel | dwarf::DW_EH_PE_sdata4;
      TTypeEncoding = dwarf::DW_EH_PE_indirect | dwarf::DW_EH_PE_pcrel |
        dwarf::DW_EH_PE_sdata4;
    } else {
      PersonalityEncoding = dwarf::DW_EH_PE_absptr;
      LSDAEncoding = dwarf::DW_EH_PE_absptr;
      TTypeEncoding = dwarf::DW_EH_PE_absptr;
    }
    break;
  default:
    break;
  }

  unsigned EHSectionType = T.getArch() == Triple::x86_64
                               ? ELF::SHT_X86_64_UNWIND
                               : ELF::SHT_PROGBITS;

  // Solaris requires different flags for .eh_frame to seemingly every other
  // platform.
  unsigned EHSectionFlags = ELF::SHF_ALLOC;
  if (T.isOSSolaris() && T.getArch() != Triple::x86_64)
    EHSectionFlags |= ELF::SHF_WRITE;

  // ELF
  BSSSection = Ctx->getELFSection(".bss", ELF::SHT_NOBITS,
                                  ELF::SHF_WRITE | ELF::SHF_ALLOC);

  TextSection = Ctx->getELFSection(".text", ELF::SHT_PROGBITS,
                                   ELF::SHF_EXECINSTR | ELF::SHF_ALLOC);

  DataSection = Ctx->getELFSection(".data", ELF::SHT_PROGBITS,
                                   ELF::SHF_WRITE | ELF::SHF_ALLOC);

  ReadOnlySection =
      Ctx->getELFSection(".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC);

  TLSDataSection =
      Ctx->getELFSection(".tdata", ELF::SHT_PROGBITS,
                         ELF::SHF_ALLOC | ELF::SHF_TLS | ELF::SHF_WRITE);

  TLSBSSSection = Ctx->getELFSection(
      ".tbss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_TLS | ELF::SHF_WRITE);

  DataRelROSection = Ctx->getELFSection(".data.rel.ro", ELF::SHT_PROGBITS,
                                        ELF::SHF_ALLOC | ELF::SHF_WRITE);

  MergeableConst4Section =
      Ctx->getELFSection(".rodata.cst4", ELF::SHT_PROGBITS,
                         ELF::SHF_ALLOC | ELF::SHF_MERGE, 4, "");

  MergeableConst8Section =
      Ctx->getELFSection(".rodata.cst8", ELF::SHT_PROGBITS,
                         ELF::SHF_ALLOC | ELF::SHF_MERGE, 8, "");

  MergeableConst16Section =
      Ctx->getELFSection(".rodata.cst16", ELF::SHT_PROGBITS,
                         ELF::SHF_ALLOC | ELF::SHF_MERGE, 16, "");

  MergeableConst32Section =
      Ctx->getELFSection(".rodata.cst32", ELF::SHT_PROGBITS,
                         ELF::SHF_ALLOC | ELF::SHF_MERGE, 32, "");

  // Exception Handling Sections.

  // FIXME: We're emitting LSDA info into a readonly section on ELF, even though
  // it contains relocatable pointers.  In PIC mode, this is probably a big
  // runtime hit for C++ apps.  Either the contents of the LSDA need to be
  // adjusted or this should be a data section.
  LSDASection = Ctx->getELFSection(".gcc_except_table", ELF::SHT_PROGBITS,
                                   ELF::SHF_ALLOC);

  COFFDebugSymbolsSection = nullptr;
  COFFDebugTypesSection = nullptr;

  unsigned DebugSecType = ELF::SHT_PROGBITS;

  // MIPS .debug_* sections should have SHT_MIPS_DWARF section type
  // to distinguish among sections contain DWARF and ECOFF debug formats.
  // Sections with ECOFF debug format are obsoleted and marked by SHT_PROGBITS.
  if (T.getArch() == Triple::mips || T.getArch() == Triple::mipsel ||
      T.getArch() == Triple::mips64 || T.getArch() == Triple::mips64el)
    DebugSecType = ELF::SHT_MIPS_DWARF;

  // Debug Info Sections.
  DwarfAbbrevSection =
      Ctx->getELFSection(".debug_abbrev", DebugSecType, 0);
  DwarfInfoSection = Ctx->getELFSection(".debug_info", DebugSecType, 0);
  DwarfLineSection = Ctx->getELFSection(".debug_line", DebugSecType, 0);
  DwarfFrameSection = Ctx->getELFSection(".debug_frame", DebugSecType, 0);
  DwarfPubNamesSection =
      Ctx->getELFSection(".debug_pubnames", DebugSecType, 0);
  DwarfPubTypesSection =
      Ctx->getELFSection(".debug_pubtypes", DebugSecType, 0);
  DwarfGnuPubNamesSection =
      Ctx->getELFSection(".debug_gnu_pubnames", DebugSecType, 0);
  DwarfGnuPubTypesSection =
      Ctx->getELFSection(".debug_gnu_pubtypes", DebugSecType, 0);
  DwarfStrSection =
      Ctx->getELFSection(".debug_str", DebugSecType,
                         ELF::SHF_MERGE | ELF::SHF_STRINGS, 1, "");
  DwarfLocSection = Ctx->getELFSection(".debug_loc", DebugSecType, 0);
  DwarfARangesSection =
      Ctx->getELFSection(".debug_aranges", DebugSecType, 0);
  DwarfRangesSection =
      Ctx->getELFSection(".debug_ranges", DebugSecType, 0);
  DwarfMacinfoSection =
      Ctx->getELFSection(".debug_macinfo", DebugSecType, 0);

  // DWARF5 Experimental Debug Info

  // Accelerator Tables
  DwarfAccelNamesSection =
      Ctx->getELFSection(".apple_names", ELF::SHT_PROGBITS, 0);
  DwarfAccelObjCSection =
      Ctx->getELFSection(".apple_objc", ELF::SHT_PROGBITS, 0);
  DwarfAccelNamespaceSection =
      Ctx->getELFSection(".apple_namespaces", ELF::SHT_PROGBITS, 0);
  DwarfAccelTypesSection =
      Ctx->getELFSection(".apple_types", ELF::SHT_PROGBITS, 0);

  // String Offset and Address Sections
  DwarfStrOffSection =
      Ctx->getELFSection(".debug_str_offsets", DebugSecType, 0);
  DwarfAddrSection = Ctx->getELFSection(".debug_addr", DebugSecType, 0);

  // Fission Sections
  DwarfInfoDWOSection =
      Ctx->getELFSection(".debug_info.dwo", DebugSecType, 0);
  DwarfTypesDWOSection =
      Ctx->getELFSection(".debug_types.dwo", DebugSecType, 0);
  DwarfAbbrevDWOSection =
      Ctx->getELFSection(".debug_abbrev.dwo", DebugSecType, 0);
  DwarfStrDWOSection =
      Ctx->getELFSection(".debug_str.dwo", DebugSecType,
                         ELF::SHF_MERGE | ELF::SHF_STRINGS, 1, "");
  DwarfLineDWOSection =
      Ctx->getELFSection(".debug_line.dwo", DebugSecType, 0);
  DwarfLocDWOSection =
      Ctx->getELFSection(".debug_loc.dwo", DebugSecType, 0);
  DwarfStrOffDWOSection =
      Ctx->getELFSection(".debug_str_offsets.dwo", DebugSecType, 0);

  // DWP Sections
  DwarfCUIndexSection =
      Ctx->getELFSection(".debug_cu_index", DebugSecType, 0);
  DwarfTUIndexSection =
      Ctx->getELFSection(".debug_tu_index", DebugSecType, 0);

  StackMapSection =
      Ctx->getELFSection(".llvm_stackmaps", ELF::SHT_PROGBITS, ELF::SHF_ALLOC);

  FaultMapSection =
      Ctx->getELFSection(".llvm_faultmaps", ELF::SHT_PROGBITS, ELF::SHF_ALLOC);

  EHFrameSection =
      Ctx->getELFSection(".eh_frame", EHSectionType, EHSectionFlags);
}

void MCObjectFileInfo::initCOFFMCObjectFileInfo(const Triple &T) {
  EHFrameSection = Ctx->getCOFFSection(
      ".eh_frame", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
                       COFF::IMAGE_SCN_MEM_READ | COFF::IMAGE_SCN_MEM_WRITE,
      SectionKind::getData());

  // Set the `IMAGE_SCN_MEM_16BIT` flag when compiling for thumb mode.  This is
  // used to indicate to the linker that the text segment contains thumb instructions
  // and to set the ISA selection bit for calls accordingly.
  const bool IsThumb = T.getArch() == Triple::thumb;

  CommDirectiveSupportsAlignment = true;

  // COFF
  BSSSection = Ctx->getCOFFSection(
      ".bss", COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA |
                  COFF::IMAGE_SCN_MEM_READ | COFF::IMAGE_SCN_MEM_WRITE,
      SectionKind::getBSS());
  TextSection = Ctx->getCOFFSection(
      ".text",
      (IsThumb ? COFF::IMAGE_SCN_MEM_16BIT : (COFF::SectionCharacteristics)0) |
          COFF::IMAGE_SCN_CNT_CODE | COFF::IMAGE_SCN_MEM_EXECUTE |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getText());
  DataSection = Ctx->getCOFFSection(
      ".data", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA | COFF::IMAGE_SCN_MEM_READ |
                   COFF::IMAGE_SCN_MEM_WRITE,
      SectionKind::getData());
  ReadOnlySection = Ctx->getCOFFSection(
      ".rdata", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA | COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getReadOnly());

  // FIXME: We're emitting LSDA info into a readonly section on COFF, even
  // though it contains relocatable pointers.  In PIC mode, this is probably a
  // big runtime hit for C++ apps.  Either the contents of the LSDA need to be
  // adjusted or this should be a data section.
  if (T.getArch() == Triple::x86_64) {
    // On Windows 64 with SEH, the LSDA is emitted into the .xdata section
    LSDASection = nullptr;
  } else {
    LSDASection = Ctx->getCOFFSection(".gcc_except_table",
                                      COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
                                          COFF::IMAGE_SCN_MEM_READ,
                                      SectionKind::getReadOnly());
  }

  // Debug info.
  COFFDebugSymbolsSection =
      Ctx->getCOFFSection(".debug$S", (COFF::IMAGE_SCN_MEM_DISCARDABLE |
                                       COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
                                       COFF::IMAGE_SCN_MEM_READ),
                          SectionKind::getMetadata());
  COFFDebugTypesSection =
      Ctx->getCOFFSection(".debug$T", (COFF::IMAGE_SCN_MEM_DISCARDABLE |
                                       COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
                                       COFF::IMAGE_SCN_MEM_READ),
                          SectionKind::getMetadata());

  DwarfAbbrevSection = Ctx->getCOFFSection(
      ".debug_abbrev",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_abbrev");
  DwarfInfoSection = Ctx->getCOFFSection(
      ".debug_info",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_info");
  DwarfLineSection = Ctx->getCOFFSection(
      ".debug_line",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_line");

  DwarfFrameSection = Ctx->getCOFFSection(
      ".debug_frame",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfPubNamesSection = Ctx->getCOFFSection(
      ".debug_pubnames",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfPubTypesSection = Ctx->getCOFFSection(
      ".debug_pubtypes",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfGnuPubNamesSection = Ctx->getCOFFSection(
      ".debug_gnu_pubnames",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfGnuPubTypesSection = Ctx->getCOFFSection(
      ".debug_gnu_pubtypes",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfStrSection = Ctx->getCOFFSection(
      ".debug_str",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "info_string");
  DwarfStrOffSection = Ctx->getCOFFSection(
      ".debug_str_offsets",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_str_off");
  DwarfLocSection = Ctx->getCOFFSection(
      ".debug_loc",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_debug_loc");
  DwarfARangesSection = Ctx->getCOFFSection(
      ".debug_aranges",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfRangesSection = Ctx->getCOFFSection(
      ".debug_ranges",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "debug_range");
  DwarfMacinfoSection = Ctx->getCOFFSection(
      ".debug_macinfo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "debug_macinfo");
  DwarfInfoDWOSection = Ctx->getCOFFSection(
      ".debug_info.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_info_dwo");
  DwarfTypesDWOSection = Ctx->getCOFFSection(
      ".debug_types.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_types_dwo");
  DwarfAbbrevDWOSection = Ctx->getCOFFSection(
      ".debug_abbrev.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_abbrev_dwo");
  DwarfStrDWOSection = Ctx->getCOFFSection(
      ".debug_str.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "skel_string");
  DwarfLineDWOSection = Ctx->getCOFFSection(
      ".debug_line.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfLocDWOSection = Ctx->getCOFFSection(
      ".debug_loc.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "skel_loc");
  DwarfStrOffDWOSection = Ctx->getCOFFSection(
      ".debug_str_offsets.dwo",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "section_str_off_dwo");
  DwarfAddrSection = Ctx->getCOFFSection(
      ".debug_addr",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "addr_sec");
  DwarfCUIndexSection = Ctx->getCOFFSection(
      ".debug_cu_index",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfTUIndexSection = Ctx->getCOFFSection(
      ".debug_tu_index",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata());
  DwarfAccelNamesSection = Ctx->getCOFFSection(
      ".apple_names",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "names_begin");
  DwarfAccelNamespaceSection = Ctx->getCOFFSection(
      ".apple_namespaces",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "namespac_begin");
  DwarfAccelTypesSection = Ctx->getCOFFSection(
      ".apple_types",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "types_begin");
  DwarfAccelObjCSection = Ctx->getCOFFSection(
      ".apple_objc",
      COFF::IMAGE_SCN_MEM_DISCARDABLE | COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
          COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getMetadata(), "objc_begin");

  DrectveSection = Ctx->getCOFFSection(
      ".drectve", COFF::IMAGE_SCN_LNK_INFO | COFF::IMAGE_SCN_LNK_REMOVE,
      SectionKind::getMetadata());

  PDataSection = Ctx->getCOFFSection(
      ".pdata", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA | COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getData());

  XDataSection = Ctx->getCOFFSection(
      ".xdata", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA | COFF::IMAGE_SCN_MEM_READ,
      SectionKind::getData());

  SXDataSection = Ctx->getCOFFSection(".sxdata", COFF::IMAGE_SCN_LNK_INFO,
                                      SectionKind::getMetadata());

  TLSDataSection = Ctx->getCOFFSection(
      ".tls$", COFF::IMAGE_SCN_CNT_INITIALIZED_DATA | COFF::IMAGE_SCN_MEM_READ |
                   COFF::IMAGE_SCN_MEM_WRITE,
      SectionKind::getData());

  StackMapSection = Ctx->getCOFFSection(".llvm_stackmaps",
                                        COFF::IMAGE_SCN_CNT_INITIALIZED_DATA |
                                            COFF::IMAGE_SCN_MEM_READ,
                                        SectionKind::getReadOnly());
}

void MCObjectFileInfo::initWasmMCObjectFileInfo(const Triple &T) {
  // TODO: Set the section types and flags.
  TextSection = Ctx->getWasmSection(".text", wasm::WASM_SEC_CODE);
  DataSection = Ctx->getWasmSection(".data", wasm::WASM_SEC_DATA);

  // TODO: Set the section types and flags.
  DwarfLineSection = Ctx->getWasmSection(".debug_line", wasm::WASM_SEC_DATA);
  DwarfStrSection = Ctx->getWasmSection(".debug_str", wasm::WASM_SEC_DATA);
  DwarfLocSection = Ctx->getWasmSection(".debug_loc", wasm::WASM_SEC_DATA);
  DwarfAbbrevSection = Ctx->getWasmSection(".debug_abbrev", wasm::WASM_SEC_DATA, "section_abbrev");
  DwarfARangesSection = Ctx->getWasmSection(".debug_aranges", wasm::WASM_SEC_DATA);
  DwarfRangesSection = Ctx->getWasmSection(".debug_ranges", wasm::WASM_SEC_DATA, "debug_range");
  DwarfMacinfoSection = Ctx->getWasmSection(".debug_macinfo", wasm::WASM_SEC_DATA, "debug_macinfo");
  DwarfAddrSection = Ctx->getWasmSection(".debug_addr", wasm::WASM_SEC_DATA);
  DwarfCUIndexSection = Ctx->getWasmSection(".debug_cu_index", wasm::WASM_SEC_DATA);
  DwarfTUIndexSection = Ctx->getWasmSection(".debug_tu_index", wasm::WASM_SEC_DATA);
  DwarfInfoSection = Ctx->getWasmSection(".debug_info", wasm::WASM_SEC_DATA, "section_info");
  DwarfFrameSection = Ctx->getWasmSection(".debug_frame", wasm::WASM_SEC_DATA);
  DwarfPubNamesSection = Ctx->getWasmSection(".debug_pubnames", wasm::WASM_SEC_DATA);
  DwarfPubTypesSection = Ctx->getWasmSection(".debug_pubtypes", wasm::WASM_SEC_DATA);

  // TODO: Define more sections.
}

void MCObjectFileInfo::initOMFMCObjectFileInfo(const Triple &T) {
  TextSection = Ctx->getOMFSection("CODE", SectionKind::getText());
  DataSection = Ctx->getOMFSection("DATA", SectionKind::getData());
  BSSSection = Ctx->getOMFSection("BSS", SectionKind::getBSS());
  ReadOnlySection = Ctx->getOMFSection("TEXT", SectionKind::getReadOnly());
}

void MCObjectFileInfo::InitMCObjectFileInfo(const Triple &TheTriple, bool PIC,
                                            MCContext &ctx,
                                            bool LargeCodeModel) {
  PositionIndependent = PIC;
  Ctx = &ctx;

  // Common.
  CommDirectiveSupportsAlignment = true;
  SupportsWeakOmittedEHFrame = true;
  SupportsCompactUnwindWithoutEHFrame = false;
  OmitDwarfIfHaveCompactUnwind = false;

  PersonalityEncoding = LSDAEncoding = FDECFIEncoding = TTypeEncoding =
      dwarf::DW_EH_PE_absptr;

  CompactUnwindDwarfEHFrameOnly = 0;

  EHFrameSection = nullptr;             // Created on demand.
  CompactUnwindSection = nullptr;       // Used only by selected targets.
  DwarfAccelNamesSection = nullptr;     // Used only by selected targets.
  DwarfAccelObjCSection = nullptr;      // Used only by selected targets.
  DwarfAccelNamespaceSection = nullptr; // Used only by selected targets.
  DwarfAccelTypesSection = nullptr;     // Used only by selected targets.

  TT = TheTriple;

  switch (TT.getObjectFormat()) {
  case Triple::MachO:
    Env = IsMachO;
    initMachOMCObjectFileInfo(TT);
    break;
  case Triple::COFF:
    if (!TT.isOSWindows())
      report_fatal_error(
          "Cannot initialize MC for non-Windows COFF object files.");

    Env = IsCOFF;
    initCOFFMCObjectFileInfo(TT);
    break;
  case Triple::ELF:
    Env = IsELF;
    initELFMCObjectFileInfo(TT, LargeCodeModel);
    break;
  case Triple::Wasm:
    Env = IsWasm;
    initWasmMCObjectFileInfo(TT);
    break;
  case Triple::OMF:
    Env = IsOMF;
    initOMFMCObjectFileInfo(TT);
    break;
  case Triple::UnknownObjectFormat:
    report_fatal_error("Cannot initialize MC for unknown object file format.");
    break;
  }
}

MCSection *MCObjectFileInfo::getDwarfTypesSection(uint64_t Hash) const {
  return Ctx->getELFSection(".debug_types", ELF::SHT_PROGBITS, ELF::SHF_GROUP,
                            0, utostr(Hash));
}
