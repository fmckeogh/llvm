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
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
using namespace llvm;

void Z80MCAsmInfo::anchor() { }

Z80MCAsmInfo::Z80MCAsmInfo(const Triple &T) {
  bool Is16Bit = T.isArch16Bit() || T.getEnvironment() == Triple::CODE16;
  PointerSize = CalleeSaveStackSlotSize = Is16Bit ? 2 : 3;
  MaxInstLength = 6;
  DollarIsPC = true;
  SeparatorString = nullptr;
  CommentString = ";";
  PrivateGlobalPrefix = PrivateLabelPrefix = "";
  Code16Directive = ".assume\tadl = 0";
  Code24Directive = ".assume\tadl = 1";
  Code32Directive = Code64Directive = nullptr;
  AssemblerDialect = !Is16Bit;
  SupportsQuotedNames = false;
  ZeroDirective = "\t.block\t";
  AvoidAsciiNull = true;
  Data16bitsDirective = "\t.word\t";
  Data24bitsDirective = "\t.word24\t";
  Data64bitsDirective = nullptr;
  AssignmentDirective = " .equ ";
  GlobalDirective = "\t.global\t";
  HasFunctionAlignment = false;
  HasDotTypeDotSizeDirective = false;
  WeakDirective = nullptr;
  UseIntegratedAssembler = false;
  WeakDirective = nullptr;
  UseLogicalShr = false;
}
