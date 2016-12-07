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

/// Return a pass that optimizes z80 call sequences.
FunctionPass *createZ80CallFrameOptimization();

/// Return a Machine IR pass that expands Z80-specific pseudo
/// instructions into a sequence of actual instructions. This pass
/// must run after prologue/epilogue insertion and before lowering
/// the MachineInstr to MC.
FunctionPass *createZ80ExpandPseudoPass();

/// Return a pass that optimizes instructions after register selection.
FunctionPass *createZ80MachineLateOptimization();
} // End llvm namespace

#endif
