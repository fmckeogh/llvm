//===----- Z80CallFrameOptimization.cpp - Optimize z80 call sequences -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a pass that optimizes call sequences on z80.
//
//===----------------------------------------------------------------------===//

#include "Z80.h"
#include "llvm/CodeGen/MachineFunctionPass.h"

using namespace llvm;

#define DEBUG_TYPE "z80-cf-opt"

static cl::opt<bool>
    NoZ80CFOpt("no-z80-call-frame-opt",
               cl::desc("Avoid optimizing z80 call frames"),
               cl::init(false), cl::Hidden);

namespace {
class Z80CallFrameOptimization : public MachineFunctionPass {
public:
  Z80CallFrameOptimization() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  static char ID;
};

char Z80CallFrameOptimization::ID = 0;
} // end anonymous namespace

FunctionPass *llvm::createZ80CallFrameOptimization() {
  return new Z80CallFrameOptimization();
}

bool Z80CallFrameOptimization::runOnMachineFunction(MachineFunction &MF) {
  if (skipFunction(*MF.getFunction()) || NoZ80CFOpt.getValue())
    return false;
  return false;
}
