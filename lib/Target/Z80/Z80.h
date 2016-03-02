//===-- Z80.h - Top-level interface for Z80 representation ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// Z80 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_Z80_H
#define LLVM_LIB_TARGET_Z80_Z80_H

#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "llvm/Support/CodeGen.h"

namespace llvm {
class FunctionPass;
class Z80TargetMachine;

/// This pass converts a legalized DAG into a Z80-specific DAG, ready for
/// instruction scheduling.
 FunctionPass *createZ80ISelDag(Z80TargetMachine &TM,
				CodeGenOpt::Level OptLevel);
} // End llvm namespace

#endif
