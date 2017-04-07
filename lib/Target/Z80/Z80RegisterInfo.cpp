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
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetFrameLowering.h"
using namespace llvm;

#define DEBUG_TYPE "z80reginfo"

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
  switch (Kind) {
  default: llvm_unreachable("Unexpected Kind in getPointerRegClass!");
  case 0: return Is24Bit ? &Z80::G24RegClass : &Z80::G16RegClass;
  case 1: return Is24Bit ? &Z80::A24RegClass : &Z80::A16RegClass;
  case 2: return Is24Bit ? &Z80::I24RegClass : &Z80::I16RegClass;
  }
}

const TargetRegisterClass *
Z80RegisterInfo::getLargestLegalSuperClass(const TargetRegisterClass *RC,
                                           const MachineFunction &) const {
  const TargetRegisterClass *Super = RC;
  TargetRegisterClass::sc_iterator I = RC->getSuperClasses();
  do {
    switch (Super->getID()) {
    case Z80::R8RegClassID:
    case Z80::R16RegClassID:
    case Z80::R24RegClassID:
      return Super;
    }
    Super = *I++;
  } while (Super);
  return RC;
}

unsigned Z80RegisterInfo::getRegPressureLimit(const TargetRegisterClass *RC,
                                              MachineFunction &MF) const {
  return 3;
  const Z80FrameLowering *TFI = getFrameLowering(MF);

  switch (RC->getID()) {
  default:
    return 0;
  case Z80::R16RegClassID:
  case Z80::R24RegClassID:
    return 2;
  }
}

const MCPhysReg *
Z80RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  switch (MF->getFunction()->getCallingConv()) {
  default: llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    return Is24Bit ? CSR_EZ80_C_SaveList : CSR_Z80_C_SaveList;
  case CallingConv::PreserveAll:
  case CallingConv::Z80_LibCall:
  case CallingConv::Z80_LibCall_AC:
  case CallingConv::Z80_LibCall_BC:
  case CallingConv::Z80_LibCall_C:
  case CallingConv::Z80_LibCall_L:
    return Is24Bit ? CSR_EZ80_AllRegs_SaveList : CSR_Z80_AllRegs_SaveList;
  }
}

const uint32_t *
Z80RegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                      CallingConv::ID CC) const {
  switch (CC) {
  default: llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    return Is24Bit ? CSR_EZ80_C_RegMask : CSR_Z80_C_RegMask;
  case CallingConv::PreserveAll:
  case CallingConv::Z80_LibCall:
  case CallingConv::Z80_LibCall_AC:
  case CallingConv::Z80_LibCall_BC:
  case CallingConv::Z80_LibCall_C:
  case CallingConv::Z80_LibCall_L:
    return Is24Bit ? CSR_EZ80_AllRegs_RegMask : CSR_Z80_AllRegs_RegMask;
  }
}
const uint32_t *Z80RegisterInfo::getNoPreservedMask() const {
  return CSR_NoRegs_RegMask;
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
  //if (TFI->hasFP(MF))
  for (MCSubRegIterator I(Z80::UIX, this, /*IncludesSelf=*/true); I.isValid();
       ++I)
    Reserved.set(*I);

  return Reserved;
}

bool Z80RegisterInfo::saveScavengerRegister(MachineBasicBlock &MBB,
                                            MachineBasicBlock::iterator I,
                                            MachineBasicBlock::iterator &UseMI,
                                            const TargetRegisterClass *RC,
                                            unsigned Reg) const {
  return false;
  const Z80Subtarget &STI = MBB.getParent()->getSubtarget<Z80Subtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();
  DebugLoc DL;
  BuildMI(MBB, I, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
    .addReg(Reg);
  for (MachineBasicBlock::iterator II = I; II != UseMI ; ++II) {
    if (II->isDebugValue())
      continue;
    if (II->modifiesRegister(Reg, TRI))
      UseMI = II;
  }
  BuildMI(MBB, UseMI, DL, TII.get(Is24Bit ? Z80::POP24r : Z80::POP16r), Reg);
  return true;
}

void Z80RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                          int SPAdj, unsigned FIOperandNum,
                                          RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  const Z80FrameLowering *TFI = getFrameLowering(MF);
  DebugLoc DL = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  unsigned BasePtr = getFrameRegister(MF);
  DEBUG(MF.dump(); II->dump(); dbgs() << MF.getFunction()->arg_size() << '\n');
  assert(TFI->hasFP(MF) && "Stack slot use without fp unimplemented");
  int Offset = MF.getFrameInfo().getObjectOffset(FrameIndex);
  int SlotSize = Is24Bit ? 3 : 2;
  // Skip saved frame pointer if used
  if (TFI->hasFP(MF))
    Offset += SlotSize;
  // Skip return address for arguments
  if (FrameIndex < 0)
    Offset += SlotSize;
  Offset += MI.getOperand(FIOperandNum + 1).getImm();
  if (isInt<8>(Offset)) {
    MI.getOperand(FIOperandNum).ChangeToRegister(BasePtr, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }
  unsigned ScratchReg = MRI.createVirtualRegister(Is24Bit ? &Z80::O24RegClass
                                                          : &Z80::O16RegClass);
  BuildMI(MBB, II, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
    .addReg(BasePtr);
  BuildMI(MBB, II, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri), ScratchReg)
    .addImm(Offset);
  BuildMI(MBB, II, DL, TII.get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao), BasePtr)
    .addReg(BasePtr).addReg(ScratchReg, RegState::Kill);
  MI.getOperand(FIOperandNum).ChangeToRegister(BasePtr, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
  BuildMI(MBB, ++II, DL, TII.get(Is24Bit ? Z80::POP24r : Z80::POP16r), BasePtr);
}

unsigned Z80RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return getFrameLowering(MF)->hasFP(MF) ? (Is24Bit ? Z80::UIX : Z80::IX)
                                         : (Is24Bit ? Z80::SPL : Z80::SPS);
}

bool Z80RegisterInfo::shouldCoalesce(MachineInstr *MI,
                                     const TargetRegisterClass *SrcRC,
                                     unsigned SubReg,
                                     const TargetRegisterClass *DstRC,
                                     unsigned DstSubReg,
                                     const TargetRegisterClass *NewRC) const {
  const TargetRegisterInfo &TRI = *MI->getParent()->getParent()->getRegInfo()
    .getTargetRegisterInfo();
  (void)TRI;
  DEBUG(dbgs() << TRI.getRegClassName(SrcRC) << ':'
        << (SubReg ? TRI.getSubRegIndexName(SubReg) : "") << " -> "
        << TRI.getRegClassName(DstRC) << ':'
        << (DstSubReg ? TRI.getSubRegIndexName(DstSubReg) : "") << ' '
        << TRI.getRegClassName(NewRC) << '\n');
  return true;
}
