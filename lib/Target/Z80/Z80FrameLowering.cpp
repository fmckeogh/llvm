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
  return true;
  const MachineFrameInfo &MFI = MF.getFrameInfo();

  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
    MFI.hasVarSizedObjects() || MFI.isFrameAddressTaken()
    // TODO
    || !MF.getFunction()->arg_empty();
}

void Z80FrameLowering::BuildStackAdjustment(MachineFunction &MF,
                                            MachineBasicBlock &MBB,
                                            MachineBasicBlock::iterator MI,
                                            DebugLoc DL, unsigned ScratchReg,
                                            int Offset, int FPOffset) const {
  if (!Offset)
    return;

  bool OptSize = MF.getFunction()->getAttributes()
    .hasAttribute(AttributeSet::FunctionIndex, Attribute::OptimizeForSize);
  uint32_t WordSize = Is24Bit ? 3 : 2;

  // Optimal if we are trying to set SP = FP
  //   LD SP, FP
  if (FPOffset >= 0 && FPOffset + Offset == 0) {
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
  bool CanUseLEA = STI.hasEZ80Ops() && FPOffset >= 0 &&
    isInt<8>(FPOffset + Offset) && hasFP(MF);
  unsigned LEACost = CanUseLEA ? 4 : LargeCost;

  // Prefer smaller version
  if (SmallCost <= LargeCost && SmallCost <= LEACost) {
    while (PopPushCount--)
      BuildMI(MBB, MI, DL, TII.get(Offset >= 0 ? (Is24Bit ? Z80::POP24r
                                                          : Z80::POP16r)
                                               : (Is24Bit ? Z80::PUSH24r
                                                          : Z80::PUSH16r)))
        .addReg(ScratchReg, getDefRegState(Offset >= 0) | RegState::Undef);
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
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::ADD24SP : Z80::ADD16SP),
            ScratchReg).addReg(ScratchReg);
  } else {
    assert(CanUseLEA && hasFP(MF) && "Can't use lea");
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LEA24ro : Z80::LEA16ro),
            ScratchReg).addReg(TRI->getFrameRegister(MF))
      .addImm(FPOffset + Offset);
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

  int StackSize = -(int)MF.getFrameInfo().getStackSize();
  unsigned ScratchReg = Is24Bit ? Z80::UHL : Z80::HL;
  if (MF.getFunction()->getAttributes().hasAttribute(
          AttributeSet::FunctionIndex, Attribute::OptimizeForSize)) {
    if (StackSize) {
      BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri),
              ScratchReg).addImm(StackSize);
      BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::CALL24i : Z80::CALL24r))
        .addExternalSymbol("_frameset").addReg(ScratchReg, RegState::Implicit);
      return;
    }
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::CALL24i : Z80::CALL24r))
      .addExternalSymbol("_frameset0");
    return;
  }
  if (hasFP(MF)) {
    unsigned FrameReg = TRI->getFrameRegister(MF);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::PUSH24r : Z80::PUSH16r))
      .addReg(FrameReg);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24ri : Z80::LD16ri),
            FrameReg)
      .addImm(0);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::ADD24SP : Z80::ADD16SP),
            FrameReg).addReg(FrameReg);
  }
  BuildStackAdjustment(MF, MBB, MI, DL, ScratchReg, StackSize, 0);
}

void Z80FrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL = MBB.findDebugLoc(MI);

  MachineFrameInfo &MFI = MF.getFrameInfo();
  int StackSize = (int)MFI.getStackSize();
  if (hasFP(MF)) {
    unsigned FrameReg = TRI->getFrameRegister(MF);
    if (StackSize || MFI.hasVarSizedObjects())
      BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::LD24SP : Z80::LD16SP))
        .addReg(FrameReg);
    BuildMI(MBB, MI, DL, TII.get(Is24Bit ? Z80::POP24r : Z80::POP16r),
            FrameReg);
  } else {
    assert(!MFI.hasVarSizedObjects() &&
           "Can't use StackSize with var sized objects!");
    unsigned ScratchReg = Is24Bit ? Z80::UIY : Z80::IY;
    BuildStackAdjustment(MF, MBB, MI, DL, ScratchReg, StackSize, -StackSize);
  }
}

MachineBasicBlock::iterator Z80FrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  if (!hasReservedCallFrame(MF)) {
    unsigned Amount = I->getOperand(0).getImm();
    unsigned ScratchReg = I->getOperand(I->getNumOperands() - 1).getReg();
    assert((Z80::A16RegClass.contains(ScratchReg) ||
            Z80::A24RegClass.contains(ScratchReg)) &&
           "Expected last operand to be the scratch reg.");
    if (I->getOpcode() == TII.getCallFrameSetupOpcode()) {
      Amount = -Amount;
    } else {
      assert(I->getOpcode() == TII.getCallFrameDestroyOpcode());
      Amount -= I->getOperand(1).getImm();
    }
    assert(TargetRegisterInfo::isPhysicalRegister(ScratchReg) &&
           "Reg alloc should have already happened.");
    BuildStackAdjustment(MF, MBB, I, I->getDebugLoc(), ScratchReg, Amount);
  }

  return MBB.erase(I);
}
