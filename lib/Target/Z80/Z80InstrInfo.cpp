//===-- Z80InstrInfo.cpp - Z80 Instruction Information --------------------===//
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

#include "Z80InstrInfo.h"
#include "Z80.h"
#include "Z80Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "Z80GenInstrInfo.inc"

// Pin the vtable to this file.
void Z80InstrInfo::anchor() {}

Z80InstrInfo::Z80InstrInfo(Z80Subtarget &STI)
    : Z80GenInstrInfo(), Subtarget(STI), RI(STI.getTargetTriple()) {
}

void Z80InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MI,
                                       unsigned SrcReg, bool isKill,
                                       int FrameIndex,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo *TRI) const {
  const MachineFunction &MF = *MBB.getParent();
  unsigned Opc;
  switch (RC->getSize()) {
  default: llvm_unreachable("Cannot store this register to stack slot!");
  case 1: Opc = Z80::LD8rmr;  break;
  case 2: Opc = Z80::LD16rmr; break;
  case 3: Opc = Z80::LD24rmr; break;
  }
  BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(Opc))
    .addFrameIndex(FrameIndex).addImm(0)
    .addReg(SrcReg, getKillRegState(isKill));
}

void Z80InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        unsigned DestReg, int FrameIndex,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo *TRI) const {
  const MachineFunction &MF = *MBB.getParent();
  unsigned Opc;
  switch (RC->getSize()) {
  default: llvm_unreachable("Cannot load this register from stack slot!");
  case 1: Opc = Z80::LD8rrm;  break;
  case 2: Opc = Z80::LD16rrm; break;
  case 3: Opc = Z80::LD24rrm; break;
  }
  BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(Opc), DestReg)
    .addFrameIndex(FrameIndex).addImm(0);
}
