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

namespace Z80 {
  // Z80 specific condition code. These correspond to Z80_*_COND in
  // Z80InstrInfo.td. They must be kept in synch.
enum CondCode {
  COND_NZ = 0,
  COND_Z = 1,
  COND_NC = 2,
  COND_C = 3,
  LAST_SIMPLE_COND = COND_C,

  COND_PO = 4,
  COND_PE = 5,
  COND_P = 6,
  COND_M = 7,
  LAST_VALID_COND = COND_M,

  COND_INVALID
};

/// GetOppositeBranchCondition - Return the inverse of the specified cond,
/// e.g. turning COND_Z to COND_NZ.
CondCode GetOppositeBranchCondition(CondCode CC);
} // end namespace Z80;

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

  // Branch analysis.
  bool isUnpredicatedTerminator(const MachineInstr &MI) const override;
  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify) const override;

  unsigned RemoveBranch(MachineBasicBlock &MBB) const override;
  unsigned InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL) const override;

  void getUnconditionalBranch(MCInst &Branch,
                              const MCSymbolRefExpr *Target) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                   const DebugLoc &DL, unsigned DstReg, unsigned SrcReg,
                   bool KillSrc) const override;
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI,
                           unsigned SrcReg, bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const override;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            unsigned DstReg, int FrameIndex,
                            const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const override;
  bool expandPostRAPseudo(MachineInstr &MI) const override;

  /// analyzeCompare - For a comparison instruction, return the source registers
  /// in SrcReg and SrcReg2 if having two register operands, and the value it
  /// compares against in CmpValue. Return true if the comparison instruction
  /// can be analyzed.
  bool analyzeCompare(const MachineInstr &MI, unsigned &SrcReg,
                      unsigned &SrcReg2, int &CmpMask,
                      int &CmpValue) const override;
  /// optimizeCompareInstr - Check if there exists an earlier instruction that
  /// operates on the same source operands and sets flags in the same way as
  /// Compare; remove Compare if possible.
  bool optimizeCompareInstr(MachineInstr &CmpInstr, unsigned SrcReg,
                            unsigned SrcReg2, int CmpMask, int CmpValue,
                            const MachineRegisterInfo *MRI) const override;
};

} // End llvm namespace

#endif
