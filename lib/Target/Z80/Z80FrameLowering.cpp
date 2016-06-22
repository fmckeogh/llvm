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

void Z80FrameLowering::BuildStackAdjustment(MachineFunction &MF,
                                            MachineBasicBlock &MBB,
                                            MachineBasicBlock::iterator MI,
                                            DebugLoc DL, int32_t Offset,
                                            int32_t FPOffsetFromSP) const {
  if (!Offset)
    return;

  bool OptSize = MF.getFunction()->getAttributes()
    .hasAttribute(AttributeSet::FunctionIndex, Attribute::OptimizeForSize);
  unsigned ScratchReg = Is24Bit ? Z80::UHL : Z80::HL;
  uint32_t WordSize = Is24Bit ? 3 : 2;

  // Optimal if we are trying to set SP = FP
  //   LD SP, FP
  if (FPOffsetFromSP >= 0 && FPOffsetFromSP + Offset == 0) {
    assert(hasFP(MF) && "This function doesn't have a frame pointer");
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24SP : Z80::LD16SP))
      .addReg(TRI->getFrameRegister(MF));
    return;
  }

  // Optimal for small offsets
  //   POP/PUSH HL for every WordSize bytes
  unsigned SmallCost = OptSize ? 1 : Is24Bit ? 4 : Offset >= 0 ? 10 : 11;
  uint32_t PopPushCount = std::abs(Offset) / WordSize;
  SmallCost *= PopPushCount;
  //   INC/DEC SP for remaining bytes
  uint32_t IncDecCount = std::abs(Offset) % WordSize;
  SmallCost += (OptSize || Is24Bit ? 1 : 6) * IncDecCount;

  // Optimal for large offsets
  //   LD HL, Offset
  unsigned LargeCost = OptSize || Is24Bit ? 1 + WordSize : 10;
  //   ADD HL, SP
  LargeCost += OptSize || Is24Bit ? 1 : 11;
  //   LD SP, HL
  LargeCost += OptSize || Is24Bit ? 1 : 6;

  // Optimal for large offsets when possible
  //   LEA HL, FP + SPOffsetFromFP + Offset
  //   LD SP, HL
  bool CanUseLEA = STI.hasEZ80Ops() && FPOffsetFromSP >= 0 &&
      isInt<8>(FPOffsetFromSP + Offset);
  unsigned LEACost = CanUseLEA ? 4 : LargeCost;

  // Prefer smaller version
  if (SmallCost <= LargeCost && SmallCost <= LEACost) {
    while (PopPushCount--)
      BuildMI(MBB, MI, DL, TII.get(Offset >= 0 ? (Is24Bit ? Z80::POP24r
                                                            : Z80::POP16r)
                                                 : (Is24Bit ? Z80::PUSH24r
                                                            : Z80::PUSH16r)))
        .addReg(ScratchReg, getDefRegState(Offset >= 0));
    unsigned StackReg = Is24Bit ? Z80::SPL : Z80::SPS;
    while (IncDecCount--)
      BuildMI(MBB, MI, DL, TII.get(Offset >= 0 ? (Is24Bit ? Z80::INC24r
                                                            : Z80::INC16r)
                                                 : (Is24Bit ? Z80::DEC24r
                                                            : Z80::DEC16r)),
              StackReg).addReg(StackReg);
    return;
  }

  if (LargeCost <= LEACost) {
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri),
            ScratchReg).addImm(Offset);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
            ScratchReg).addReg(ScratchReg)
      .addReg(Is24Bit ? Z80::SPL : Z80::SPS);
  } else {
    assert(CanUseLEA && hasFP(MF) && "Can't use lea");
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LEA24rr : Z80::LEA16rr),
            ScratchReg).addReg(TRI->getFrameRegister(MF))
      .addImm(FPOffsetFromSP + Offset);
  }
  BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24SP : Z80::LD16SP))
    .addReg(ScratchReg);
}

/// emitPrologue - Push callee-saved registers onto the stack, which
/// automatically adjust the stack pointer. Adjust the stack pointer to allocate
/// space for local variables.
void Z80FrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MI = MBB.begin();

  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc DL;

  if (hasFP(MF)) {
    unsigned FrameReg = TRI->getFrameRegister(MF);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
      .addReg(FrameReg);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri))
      .addReg(FrameReg).addImm(0);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
            FrameReg).addReg(FrameReg).addReg(Is24Bit ? Z80::SPL : Z80::SPS);
  }
  int32_t StackSize = (int32_t)MF.getFrameInfo()->getStackSize();
  BuildStackAdjustment(MF, MBB, MI, DL, -StackSize, 0);
}

void Z80FrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  assert(MI->getOpcode() == Z80::RET && "Can only emit epilog if returning");

  DebugLoc DL;
  if (MI != MBB.end())
    DL = MI->getDebugLoc();

  int32_t StackSize = (int32_t)MF.getFrameInfo()->getStackSize();
  if (hasFP(MF)) {
    unsigned FrameReg = TRI->getFrameRegister(MF);
    if (StackSize)
      BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24SP : Z80::LD16SP))
        .addReg(FrameReg);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::POP24r : Z80::POP16r),
            FrameReg);
  } else {
    BuildStackAdjustment(MF, MBB, MI, DL, StackSize, -StackSize);
  }
}

MachineBasicBlock::iterator Z80FrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  if (!hasReservedCallFrame(MF)) {
    // We need to keep the stack aligned properly.  To do this, we round the
    // amount of space needed for the outgoing arguments up to the next
    // alignment boundary.
    uint64_t Amount = alignTo(I->getOperand(0).getImm(), getStackAlignment());
    if (I->getOpcode() == TII.getCallFrameSetupOpcode()) {
      Amount = -Amount;
    } else {
      assert(I->getOpcode() == TII.getCallFrameDestroyOpcode());
      Amount -= I->getOperand(1).getImm();
    }
    BuildStackAdjustment(MF, MBB, I, I->getDebugLoc(), Amount);
  }

  return MBB.erase(I);
}
