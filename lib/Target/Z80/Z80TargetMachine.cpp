//===-- Z80TargetMachine.cpp - Define TargetMachine for the Z80 -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Z80 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#include "Z80TargetMachine.h"
#include "Z80.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

extern "C" void LLVMInitializeZ80Target() {
  // Register the target.
  RegisterTargetMachine<Z80TargetMachine> X(TheZ80Target);
  RegisterTargetMachine<Z80TargetMachine> Y(TheEZ80Target);
}

static std::string computeDataLayout(const Triple &TT) {
  if (TT.isArch16Bit())
    return "e-p:16:8-p1:8:8-i16:8-i32:8-a:8-n8:16";
  else
    return "e-p:24:8-p1:16:8-i16:8-i24:8-i32:8-a:8-n8:16:24";
}

/// Z80TargetMachine ctor - Create a Z80 target.
///
Z80TargetMachine::Z80TargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Reloc::Model RM, CodeModel::Model CM,
                                   CodeGenOpt::Level OL)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options, RM, CM,
                        OL),
      TLOF(make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

Z80TargetMachine::~Z80TargetMachine() {}


//===----------------------------------------------------------------------===//
// Pass Pipeline Configuration
//===----------------------------------------------------------------------===//

namespace {
/// Z80 Code Generator Pass Configuration Options.
class Z80PassConfig : public TargetPassConfig {
public:
  Z80PassConfig(Z80TargetMachine *TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  Z80TargetMachine &getZ80TargetMachine() const {
    return getTM<Z80TargetMachine>();
  }

  bool addInstSelector() override;
};
} // namespace

TargetPassConfig *Z80TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new Z80PassConfig(this, PM);
}

bool Z80PassConfig::addInstSelector() {
  // Install an instruction selector.
  addPass(createZ80ISelDag(getZ80TargetMachine(), getOptLevel()));
  return false;
}
