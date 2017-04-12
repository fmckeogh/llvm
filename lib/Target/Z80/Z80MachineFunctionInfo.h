//===-- Z80MachineFunctionInfo.h - Z80 machine function info ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares Z80-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_Z80MACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_Z80_Z80MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// Z80MachineFunctionInfo - This class is derived from MachineFunction and
/// contains private X86 target-specific information for each MachineFunction.
class Z80MachineFunctionInfo : public MachineFunctionInfo {
  virtual void anchor();

public:
  Z80MachineFunctionInfo() = default;

  explicit Z80MachineFunctionInfo(MachineFunction &MF) {}

  /// VarArgsFrameIndex - FrameIndex for start of varargs area.
  int VarArgsFrameIndex = 0;
  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Idx) { VarArgsFrameIndex = Idx; }
};

} // End llvm namespace

#endif
