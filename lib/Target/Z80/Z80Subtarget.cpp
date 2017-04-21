//===-- Z80Subtarget.cpp - Z80 Subtarget Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Z80 specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "Z80Subtarget.h"
#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "Z80FrameLowering.h"
using namespace llvm;

#define DEBUG_TYPE "z80-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "Z80GenSubtargetInfo.inc"

Z80Subtarget &Z80Subtarget::initializeSubtargetDependencies(StringRef CPU,
                                                            StringRef FS) {
  ParseSubtargetFeatures(CPU, FS);
  HasIdxHalfRegs = HasUndocOps || HasEZ80Ops;
  return *this;
}

Z80Subtarget::Z80Subtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, const Z80TargetMachine &TM)
    : Z80GenSubtargetInfo(TT, CPU, FS), TargetTriple(TT),
      In16BitMode(TT.isArch16Bit() || TT.getEnvironment() == Triple::CODE16),
      In24BitMode(!In16BitMode), HasUndocOps(false), HasZ180Ops(false),
      HasEZ80Ops(false), HasIdxHalfRegs(false),
      InstrInfo(initializeSubtargetDependencies(CPU, FS)),
      TLInfo(TM, *this), FrameLowering(*this) {
}
