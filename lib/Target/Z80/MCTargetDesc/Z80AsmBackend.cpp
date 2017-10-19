//===-- Z80AsmBackend.cpp - Z80 Assembler Backend -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Z80FixupKinds.h"
#include "Z80MCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCOMFObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace {

class Z80AsmBackend : public MCAsmBackend {
public:
  Z80AsmBackend(const Target &T)
      : MCAsmBackend() {}

  unsigned getNumFixupKinds() const override {
    return Z80::NumTargetFixupKinds;
  }

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved) const override;

  bool mayNeedRelaxation(const MCInst &Inst) const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override;

  void relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const override;

  bool writeNopData(uint64_t Count, MCObjectWriter *OW) const override {
    OW->WriteZeros(Count);
    return true;
  }
};

} // end anonymous namespace

void Z80AsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target,
                               MutableArrayRef<char> Data, uint64_t Value,
                               bool IsResolved) const {
  llvm_unreachable("Unimplemented");
}

bool Z80AsmBackend::mayNeedRelaxation(const MCInst &Inst) const {
  llvm_unreachable("Unimplemented");
}

bool Z80AsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                         const MCRelaxableFragment *DF,
                                         const MCAsmLayout &Layout) const {
  llvm_unreachable("Unimplemented");
}

void Z80AsmBackend::relaxInstruction(const MCInst &Inst,
                                     const MCSubtargetInfo &STI,
                                     MCInst &Res) const {
  llvm_unreachable("Unimplemented");
}

namespace {

class OMFZ80AsmBackend : public Z80AsmBackend {
public:
  OMFZ80AsmBackend(const Target &T) : Z80AsmBackend(T) {}

  std::unique_ptr<MCObjectWriter>
  createObjectWriter(raw_pwrite_stream &OS) const override {
    return createZ80OMFObjectWriter(OS);
  }
};

} // end anonymous namespace

MCAsmBackend *llvm::createZ80AsmBackend(const Target &T,
                                        const MCRegisterInfo &MRI,
                                        const Triple &TheTriple, StringRef CPU,
                                        const MCTargetOptions &Options) {
  return new OMFZ80AsmBackend(T);
}

MCAsmBackend *llvm::createEZ80AsmBackend(const Target &T,
                                         const MCRegisterInfo &MRI,
                                         const Triple &TheTriple, StringRef CPU,
                                         const MCTargetOptions &Options) {
  return new OMFZ80AsmBackend(T);
}
