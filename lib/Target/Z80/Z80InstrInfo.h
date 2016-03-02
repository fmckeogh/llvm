//===-- Z80InstrInfo.h - Z80 Instruction Information ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Z80 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_Z80INSTRINFO_H
#define LLVM_LIB_TARGET_Z80_Z80INSTRINFO_H

#include "Z80RegisterInfo.h"
#include "llvm/Target/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "Z80GenInstrInfo.inc"

namespace llvm {
class Z80Subtarget;

class Z80InstrInfo final : public Z80GenInstrInfo {
  Z80Subtarget &Subtarget;
  const Z80RegisterInfo RI;

  virtual void anchor();

public:
  explicit Z80InstrInfo(Z80Subtarget &STI);

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  ///
  const Z80RegisterInfo &getRegisterInfo() const { return RI; }

  void storeRegToStackSlot(MachineBasicBlock &MBB,
			   MachineBasicBlock::iterator MI,
			   unsigned SrcReg, bool isKill, int FrameIndex,
			   const TargetRegisterClass *RC,
			   const TargetRegisterInfo *TRI) const override;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
			    MachineBasicBlock::iterator MI,
			    unsigned SrcReg, int FrameIndex,
			    const TargetRegisterClass *RC,
			    const TargetRegisterInfo *TRI) const override;
};

} // End llvm namespace

#endif
