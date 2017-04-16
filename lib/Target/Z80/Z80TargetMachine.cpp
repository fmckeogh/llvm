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
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/Scalar.h"
using namespace llvm;

extern "C" void LLVMInitializeZ80Target() {
  // Register the target.
  RegisterTargetMachine<Z80TargetMachine> X(TheZ80Target);
  RegisterTargetMachine<Z80TargetMachine> Y(TheEZ80Target);
}

static std::string computeDataLayout(const Triple &TT) {
  bool IsEZ80 = TT.getArch() == Triple::ez80;
  bool Is16Bit = TT.isArch16Bit() || TT.getEnvironment() == Triple::CODE16;
  // Z80 is little endian and mangling is closest to MachO.
  std::string Ret = "e-m:o";
  // Memory Address Width
  Ret += Is16Bit ? "-p:16:8" : "-p:24:8";
  // Port Address Width
  Ret += IsEZ80 ? "-p1:16:8" : "-p1:8:8";
  // Other Address Width
  if (IsEZ80)
    Ret += Is16Bit ? "-p2:24:8" : "-p2:16:8";
  Ret += "-i16:8-i24:8-i32:8-i48:8-i64:8-i96:8-f32:8-f64:8-a:8-n8:16";
  if (!Is16Bit)
    Ret += ":24";
  Ret += "-S8";
  return Ret;
}

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
  if (!RM.hasValue())
    return Reloc::Static;
  return *RM;
}

/// Z80TargetMachine ctor - Create a Z80 target.
///
Z80TargetMachine::Z80TargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Optional<Reloc::Model> RM,
                                   CodeModel::Model CM, CodeGenOpt::Level OL)
  : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                      getEffectiveRelocModel(RM), CM, OL),
    TLOF(make_unique<TargetLoweringObjectFileOMF>()) {
  initAsmInfo();
}

Z80TargetMachine::~Z80TargetMachine() {}

const Z80Subtarget *
Z80TargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  StringRef CPU = !CPUAttr.hasAttribute(Attribute::None)
                      ? CPUAttr.getValueAsString()
                      : (StringRef)TargetCPU;
  StringRef FS = !FSAttr.hasAttribute(Attribute::None)
                     ? FSAttr.getValueAsString()
                     : (StringRef)TargetFS;

  SmallString<512> Key;
  Key.reserve(CPU.size() + FS.size());
  Key += CPU;
  Key += FS;

  auto &I = SubtargetMap[Key];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = llvm::make_unique<Z80Subtarget>(TargetTriple, CPU, FS, *this);
  }
  return I.get();
}

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

  void addCodeGenPrepare() override;
  bool addInstSelector() override;
//void addPreRegAlloc() override;
//bool addPreRewrite() override;
//void addPreSched2() override;
};
} // namespace

TargetPassConfig *Z80TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new Z80PassConfig(this, PM);
}

void Z80PassConfig::addCodeGenPrepare() {
  addPass(createLowerSwitchPass());
  TargetPassConfig::addCodeGenPrepare();
}

bool Z80PassConfig::addInstSelector() {
  // Install an instruction selector.
  addPass(createZ80ISelDag(getZ80TargetMachine(), getOptLevel()));
  return false;
}

/*void Z80PassConfig::addPreRegAlloc() {
  TargetPassConfig::addPreRegAlloc();
  if (getOptLevel() != CodeGenOpt::None)
    ;//addPass(createZ80CallFrameOptimization());
}

bool Z80PassConfig::addPreRewrite() {
  //addPass(createZ80ExpandPseudoPass());
  return TargetPassConfig::addPreRewrite();
}

void Z80PassConfig::addPreSched2() {
  // Z80MachineLateOptimization pass must be run after ExpandPostRAPseudos
  if (getOptLevel() != CodeGenOpt::None)
    addPass(createZ80MachineLateOptimization());
  TargetPassConfig::addPreSched2();
}
*/
