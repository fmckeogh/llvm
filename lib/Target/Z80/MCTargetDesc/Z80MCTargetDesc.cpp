//===-- Z80MCTargetDesc.cpp - Z80 Target Descriptions ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Z80 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "Z80MCTargetDesc.h"
#include "InstPrinter/Z80InstPrinter.h"
#include "Z80MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

#define GET_REGINFO_MC_DESC
#include "Z80GenRegisterInfo.inc"

#define GET_INSTRINFO_MC_DESC
#include "Z80GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "Z80GenSubtargetInfo.inc"

std::string Z80_MC::ParseZ80Triple(const Triple &TT) {
  std::string FS;
  if (TT.getArch() == Triple::ez80)
    FS = "+24bit-mode,-16bit-mode";
  else
    FS = "-24bit-mode,+16bit-mode";

  return FS;
}

MCSubtargetInfo *Z80_MC::createZ80MCSubtargetInfo(const Triple &TT,
                                                  StringRef CPU, StringRef FS) {
  std::string ArchFS = Z80_MC::ParseZ80Triple(TT);
  if (!FS.empty()) {
    if (!ArchFS.empty())
      ArchFS = (Twine(ArchFS) + "," + FS).str();
    else
      ArchFS = FS;
  }

  return createZ80MCSubtargetInfoImpl(TT, CPU, ArchFS);
}

static MCInstrInfo *createZ80MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitZ80MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createZ80MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitZ80MCRegisterInfo(X, Z80::PC);
  return X;
}

static MCAsmInfo *createZ80MCAsmInfo(const MCRegisterInfo &MRI,
                                     const Triple &TheTriple) {
  return new Z80ELFMCAsmInfo(TheTriple);
}

static MCInstPrinter *createZ80MCInstPrinter(const Triple &TT,
                                             unsigned SyntaxVariant,
                                             const MCAsmInfo &MAI,
                                             const MCInstrInfo &MII,
                                             const MCRegisterInfo &MRI) {
  if (SyntaxVariant == 0)
    return new Z80InstPrinter(MAI, MII, MRI);
  return nullptr;
}

// Force static initialization.
extern "C" void LLVMInitializeZ80TargetMC() {
  for (Target *T : {&TheZ80Target, &TheEZ80Target}) {
    // Register the MC asm info.
    RegisterMCAsmInfoFn X(*T, createZ80MCAsmInfo);

    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createZ80MCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createZ80MCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T,
                                            Z80_MC::createZ80MCSubtargetInfo);

    // Register the MCInstPrinter.
    TargetRegistry::RegisterMCInstPrinter(*T, createZ80MCInstPrinter);
  }
}
