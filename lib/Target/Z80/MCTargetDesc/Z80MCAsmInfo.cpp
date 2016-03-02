//===-- Z80MCAsmInfo.cpp - Z80 asm properties -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the Z80MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "Z80MCAsmInfo.h"
#include "llvm/ADT/Triple.h"
using namespace llvm;

void Z80ELFMCAsmInfo::anchor() { }

Z80ELFMCAsmInfo::Z80ELFMCAsmInfo(const Triple &T) {
  bool is24Bit = T.getArch() == Triple::ez80;
  PointerSize = is24Bit ? 3 : 2;
  CalleeSaveStackSlotSize = is24Bit ? 3 : 2;
  MaxInstLength = 6;
  DollarIsPC = true;
  SeparatorString = "\\";
  CommentString = ";";
  LabelSuffix = ":";
  InlineAsmStart = InlineAsmEnd = "";
  Code16Directive = "\t.assume\tadl = 0";
  Code24Directive = "\t.assume\tadl = 1";
  Code32Directive = Code64Directive = nullptr;
  SupportsQuotedNames = false;
  ZeroDirective = "\t.block\t";
  Data16bitsDirective = "\t.word\t";
  Data24bitsDirective = "\t.long\t";
  Data32bitsDirective = Data64bitsDirective = nullptr;
  GlobalDirective = ".global";
  HasDotTypeDotSizeDirective = false;
  HasIdentDirective = false;
  WeakDirective = nullptr;
  UseIntegratedAssembler = true;
  UseLogicalShr = false;
}
