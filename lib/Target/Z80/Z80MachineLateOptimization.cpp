//===----- Z80MachineLateOptimization.cpp - Optimize late machine inst -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a pass that optimizes machine instructions after register
// selection.
//
//===----------------------------------------------------------------------===//

#include "Z80.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
using namespace llvm;

#define DEBUG_TYPE "z80-ml-opt"

namespace {
class Z80MachineLateOptimization : public MachineFunctionPass {
public:
  Z80MachineLateOptimization() : MachineFunctionPass(ID) {}
protected:
  bool runOnMachineFunction(MachineFunction &MF) override;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties()
      .set(MachineFunctionProperties::Property::TracksLiveness);
  }
private:
  static char ID;
};

char Z80MachineLateOptimization::ID = 0;
} // end anonymous namespace

FunctionPass *llvm::createZ80MachineLateOptimization() {
  return new Z80MachineLateOptimization();
}

bool Z80MachineLateOptimization::runOnMachineFunction(MachineFunction &MF) {
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  assert(MF.getRegInfo().tracksLiveness() && "Liveness not being tracked!");
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();
  const TargetInstrInfo *TII = STI.getInstrInfo();
  bool Changed = false;
  for (auto &MBB : MF) {
    bool UsedFlags = false;
    for (MachineBasicBlock *Successor : MBB.successors())
      if (Successor->isLiveIn(Z80::F))
        UsedFlags = true;
    for (auto I = MBB.rbegin(), E = MBB.rend(); I != E; ++I) {
      if (!UsedFlags)
        switch (I->getOpcode()) {
        case Z80::LD8ri: // ld a, 0 -> xor a, a
          if (I->getOperand(0).getReg() == Z80::A &&
              I->getOperand(1).getImm() == 0) {
            I->setDesc(TII->get(Z80::XOR8ar));
            I->getOperand(0).setIsUse();
            I->getOperand(0).setIsUndef();
            I->RemoveOperand(1);
            DEBUG(dbgs() << '!');
            Changed = true;
          }
          break;
        case Z80::LD24ri: // ld hl, -1/0 -> set-cf \ sbc hl, hl
          if (I->getOperand(0).getReg() == Z80::UHL) {
            int Imm = I->getOperand(1).getImm();
            if (Imm == 0 || Imm == -1) {
              BuildMI(MBB, *I, I->getDebugLoc(),
                      TII->get(Imm ? Z80::SCF : Z80::RCF));
              I->setDesc(TII->get(Z80::SBC24ar));
              I->getOperand(0).setIsUse();
              I->getOperand(0).setIsUndef();
              I->RemoveOperand(1);
              DEBUG(dbgs() << '!');
              Changed = true;
            }
          }
          break;
        }
      if (I->modifiesRegister(Z80::F, TRI))
        UsedFlags = false;
      if (I->readsRegister(Z80::F, TRI))
        UsedFlags = true;
      DEBUG(dbgs() << (UsedFlags ? "true" : "false") << ":\t"; I->dump());
    }
    DEBUG(dbgs() << '\n');
  }
  return Changed;
}
