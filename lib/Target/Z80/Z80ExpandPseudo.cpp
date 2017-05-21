//===------- Z80ExpandPseudo.cpp - Expand pseudo instructions -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands pseudo instructions into target
// instructions to allow proper scheduling, if-conversion, other late
// optimizations, or simply the encoding of the instructions.
//
//===----------------------------------------------------------------------===//

#include "Z80.h"
#include "Z80InstrInfo.h"
#include "Z80Subtarget.h"
#include "../../CodeGen/LiveDebugVariables.h"
#include "../../CodeGen/SpillPlacement.h"
#include "llvm/CodeGen/CalcSpillWeights.h"
#include "llvm/CodeGen/EdgeBundles.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/LiveRangeEdit.h"
#include "llvm/CodeGen/LiveRegMatrix.h"
#include "llvm/CodeGen/LiveStackAnalysis.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/VirtRegMap.h"
using namespace llvm;

#define DEBUG_TYPE "z80-pseudo"

namespace {
class Z80ExpandPseudo : public MachineFunctionPass {
public:
  Z80ExpandPseudo() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) override;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties()
      .set(MachineFunctionProperties::Property::TracksLiveness);
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addRequired<MachineBlockFrequencyInfo>();
    AU.addPreserved<MachineBlockFrequencyInfo>();
    //AU.addRequired<AAResultsWrapperPass>();
    //AU.addPreserved<AAResultsWrapperPass>();
    AU.addRequired<LiveIntervals>();
    AU.addPreserved<LiveIntervals>();
    AU.addRequired<SlotIndexes>();
    AU.addPreserved<SlotIndexes>();
    //AU.addRequired<LiveDebugVariables>();
    //AU.addPreserved<LiveDebugVariables>();
    AU.addRequired<LiveStacks>();
    AU.addPreserved<LiveStacks>();
    AU.addRequired<MachineDominatorTree>();
    AU.addPreserved<MachineDominatorTree>();
    AU.addRequired<MachineLoopInfo>();
    AU.addPreserved<MachineLoopInfo>();
    AU.addRequired<VirtRegMap>();
    AU.addPreserved<VirtRegMap>();
    AU.addRequired<LiveRegMatrix>();
    AU.addPreserved<LiveRegMatrix>();
    AU.addRequired<EdgeBundles>();
    AU.addRequired<SpillPlacement>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override {
    return "Z80 Expand Pseudo Instructions";
  }

private:
  void ExpandCmp(MachineInstr &MI, MachineBasicBlock &MBB);
  void ExpandCmp0(MachineInstr &MI, MachineBasicBlock &MBB);
  bool ExpandMI(MachineBasicBlock::iterator &MI, MachineBasicBlock &MBB);
  bool ExpandMBB(MachineBasicBlock &MBB);

  const TargetInstrInfo *TII;
  static char ID;
};

char Z80ExpandPseudo::ID = 0;
} // end anonymous namespace

FunctionPass *llvm::createZ80ExpandPseudoPass() {
  return new Z80ExpandPseudo();
}

void Z80ExpandPseudo::ExpandCmp(MachineInstr &MI, MachineBasicBlock &MBB) {
  bool Is24Bit = MI.getOpcode() == Z80::CP24ao;
  assert((Is24Bit || MI.getOpcode() == Z80::CP16ao) && "Unexpected opcode");
  DebugLoc DL = MI.getDebugLoc();
  dbgs() << "Z80ExpandPseudo::ExpandCmp";
  MI.dump();
  //BuildMI(MBB, MI, DL, TII->get(Z80::RCF));
  //BuildMI(MBB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
  //  .addReg(MI.getOperand(0).getReg());
  //BuildMI(MBB, MI, DL, TII->get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
  //        Is24Bit ? Z80::UHL : Z80::HL).addReg(Is24Bit ? Z80::UHL : Z80::HL)
  //  .addReg(MI.getOperand(0).getReg());
}
void Z80ExpandPseudo::ExpandCmp0(MachineInstr &MI, MachineBasicBlock &MBB) {
  bool Is24Bit = MI.getOpcode() == Z80::CP24a0;
  assert((Is24Bit || MI.getOpcode() == Z80::CP16a0) && "Unexpected opcode");
  DebugLoc DL = MI.getDebugLoc();
  dbgs() << "Z80ExpandPseudo::ExpandCmp";
  MI.dump();
  //BuildMI(MBB, MI, DL, TII->get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
  //        Is24Bit ? Z80::UHL : Z80::HL).addReg(Is24Bit ? Z80::UHL : Z80::HL)
  //  .addReg(MI.getOperand(0).getReg());
  //BuildMI(MBB, MI, DL, TII->get(Z80::RCF));
  //BuildMI(MBB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
  //  .addReg(MI.getOperand(0).getReg());
}

bool Z80ExpandPseudo::ExpandMI(MachineBasicBlock::iterator &MI, MachineBasicBlock &MBB) {
  switch (MI->getOpcode()) {
  default: return false;
  case Z80::CP16ao:
  case Z80::CP24ao:
    ExpandCmp(*MI, MBB);
    break;
  case Z80::CP16a0:
  case Z80::CP24a0:
    ExpandCmp0(*MI, MBB);
    break;
  }
  return false;
  MI = MBB.erase(MI);
  return true;
}

bool Z80ExpandPseudo::ExpandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;
  for (auto I = MBB.begin(), E = MBB.end(); I != E; ++I)
    Modified |= ExpandMI(I, MBB);
  return Modified;
}

bool Z80ExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  getAnalysis<LiveIntervals>().addKillFlags(&getAnalysis<VirtRegMap>());
  TII = MF.getSubtarget().getInstrInfo();
  bool Modified = false;
  for (auto &MBB : MF)
    Modified |= ExpandMBB(MBB);
  return Modified;
}
