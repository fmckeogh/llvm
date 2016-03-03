//===-- Z80MCTargetDesc.h - Z80 Target Descriptions -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Z80 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_MCTARGETDESC_Z80MCTARGETDESC_H
#define LLVM_LIB_TARGET_Z80_MCTARGETDESC_Z80MCTARGETDESC_H

#include "llvm/Support/DataTypes.h"
#include <string>

namespace llvm {
class MCSubtargetInfo;
class Target;
class Triple;
class StringRef;

extern Target TheZ80Target, TheEZ80Target;

namespace Z80_MC {
std::string ParseZ80Triple(const Triple &TT);

/// Create a Z80 MCSubtargetInfo instance.  This is exposed so Asm parser, etc.
/// do not need to go through TargetRegistry.
MCSubtargetInfo *createZ80MCSubtargetInfo(const Triple &TT, StringRef CPU,
					  StringRef FS);
}
} // End llvm namespace

// Defines symbolic names for Z80 registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "Z80GenRegisterInfo.inc"

// Defines symbolic names for the Z80 instructions.
//
#define GET_INSTRINFO_ENUM
#include "Z80GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "Z80GenSubtargetInfo.inc"

#endif