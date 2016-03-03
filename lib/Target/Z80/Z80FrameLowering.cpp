//===-- Z80FrameLowering.cpp - Z80 Frame Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the z80 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "Z80FrameLowering.h"
#include "Z80.h"
#include "Z80InstrInfo.h"
#include "Z80Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
using namespace llvm;

Z80FrameLowering::Z80FrameLowering(const Z80Subtarget &STI)
    : TargetFrameLowering(StackGrowsDown, 1, STI.is24Bit() ? -3 : -2),
      STI(STI), TII(*STI.getInstrInfo()), TRI(STI.getRegisterInfo()),
      Is24Bit(STI.is24Bit()) {
}

/// hasFP - Return true if the specified function should have a dedicated frame
/// pointer register.  This is true if the function has variable sized allocas
/// or if frame pointer elimination is disabled.
bool Z80FrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();

  return (MF.getTarget().Options.DisableFramePointerElim(MF) ||
          MFI->hasVarSizedObjects() ||
          MFI->isFrameAddressTaken());
}

/// emitPrologue - Push callee-saved registers onto the stack, which
/// automatically adjust the stack pointer. Adjust the stack pointer to allocate
/// space for local variables.
void Z80FrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();
  unsigned FramePtr = TRI->getFrameRegister(MF);
  unsigned StackPtr = Is24Bit ? Z80::SPL : Z80::SPS;
  unsigned ScratchReg = Is24Bit ? Z80::UHL : Z80::HL;
  uint64_t StackSize = MFI->getStackSize();

  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc DL;

  if (hasFP(MF)) {
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
      .addReg(FramePtr);
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri))
      .addReg(FramePtr).addImm(0);
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::ADD24rrx : Z80::ADD16rrx))
      .addReg(StackPtr);
  }
  switch (StackSize) {
  default:
    if (hasFP(MF)) {
      BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::LEA24rr : Z80::LEA16rr))
        .addReg(ScratchReg).addReg(FramePtr).addImm(-StackSize);
    } else if (StackSize) {
      BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri))
        .addReg(ScratchReg).addImm(-StackSize);
      BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::ADD24rr : Z80::ADD16rr))
        .addReg(StackPtr);
    }
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::LD24sp : Z80::LD16sp))
      .addReg(ScratchReg);
    break;
  case 3:
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
      .addReg(ScratchReg);
    break;
  case 2:
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::DEC24r : Z80::DEC16r),
            StackPtr).addReg(StackPtr);
    // Fallthrough
  case 1:
    BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::DEC24r : Z80::DEC16r),
            StackPtr).addReg(StackPtr);
    break;
  case 0:
    break;
  }
}

void Z80FrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  if (!hasFP(MF))
    return;
  MachineBasicBlock::iterator MBBI = MBB.getFirstTerminator();
  assert(MBBI->getOpcode() == Z80::RET && "Can only emit epilog if returning");
  unsigned FrameReg = Is24Bit ? Z80::UIX : Z80::IX;

  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::LD24sp : Z80::LD16sp))
    .addReg(FrameReg);
  BuildMI(MBB, MBBI, DL, TII.get(Is24Bit ? Z80::POP24r : Z80::POP16r),
          FrameReg);
}
