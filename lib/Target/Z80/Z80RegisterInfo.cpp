//===-- Z80RegisterInfo.cpp - Z80 Register Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Z80 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "Z80RegisterInfo.h"
#include "Z80FrameLowering.h"
#include "Z80Subtarget.h"
#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetFrameLowering.h"
using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "Z80GenRegisterInfo.inc"

Z80RegisterInfo::Z80RegisterInfo(const Triple &TT)
    : Z80GenRegisterInfo(Z80::PC) {
  // Cache some information
  Is24Bit = !TT.isArch16Bit();
}

const TargetRegisterClass *
Z80RegisterInfo::getPointerRegClass(const MachineFunction &MF,
				    unsigned Kind) const {
  const Z80Subtarget &Subtarget = MF.getSubtarget<Z80Subtarget>();
  switch (Kind) {
  default: llvm_unreachable("Unexpected Kind in getPointerRegClass!");
  case 0:
    if (Is24Bit)
      return &Z80::G24RegClass;
    return &Z80::G16RegClass;
  }
}

const MCPhysReg *
Z80RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return Is24Bit ? CSR_EZ80_C_SaveList : CSR_Z80_C_SaveList;
}

BitVector Z80RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const Z80FrameLowering *TFI = getFrameLowering(MF);

  // Set the stack-pointer registers as reserved.
  Reserved.set(Z80::SPS);
  Reserved.set(Z80::SPL);

  // Set the program-counter register as reserved.
  Reserved.set(Z80::PC);

  // Set the frame-pointer register and its aliases as reserved if needed.
  if (TFI->hasFP(MF)) {
    for (MCSubRegIterator I(Z80::UIX, this, /*IncludesSelf=*/true); I.isValid();
	 ++I)
      Reserved.set(*I);
  }

  return Reserved;
}

void Z80RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
					  int SPAdj, unsigned FIOperandNum,
					  RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  const Z80FrameLowering *TFI = getFrameLowering(MF);
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  unsigned BasePtr = getFrameRegister(MF);
  assert(TFI->hasFP(MF) && "Stack slot use without fp unimplemented");
  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex);
  int SlotSize = Is24Bit ? 3 : 2;
  Offset += SlotSize;
  if (TFI->hasFP(MF))
    Offset += SlotSize;
  Offset += MI.getOperand(FIOperandNum + 1).getImm();
  MI.getOperand(FIOperandNum).ChangeToRegister(BasePtr, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
}

unsigned Z80RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return getFrameLowering(MF)->hasFP(MF) ? (Is24Bit ? Z80::UIX : Z80::IX)
                                         : (Is24Bit ? Z80::SPL : Z80::SPS);
}
