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
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace {

class Z80ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  Z80ELFObjectWriter(bool Is24Bit)
      : MCELFObjectTargetWriter(/*Is64Bit*/ false, 0, ELF::EM_Z80,
                                /*HasRelocationAddend*/ false) {}
};

class Z80AsmBackend : public MCAsmBackend {
public:
  Z80AsmBackend(const Target &T)
      : MCAsmBackend() {}

  unsigned getNumFixupKinds() const override {
    return Z80::NumTargetFixupKinds;
  }

  void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                  uint64_t Value, bool IsPCRel) const override;

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

/* *** */

namespace {

class ELFZ80AsmBackend : public Z80AsmBackend {
public:
  uint8_t OSABI;
  ELFZ80AsmBackend(const Target &T, uint8_t OSABI = 0)
      : Z80AsmBackend(T), OSABI(OSABI) {}

  MCObjectWriter *createObjectWriter(raw_pwrite_stream &OS) const override {
    return createZ80ELFObjectWriter(OS, OSABI);
  }
};

} // end anonymous namespace

MCAsmBackend *llvm::createZ80AsmBackend(const Target &T,
                                        const MCRegisterInfo &MRI,
                                        const Triple &TheTriple, StringRef CPU,
                                        const MCTargetOptions &Options) {
  return new ELFZ80AsmBackend(T);
}

MCAsmBackend *llvm::createEZ80AsmBackend(const Target &T,
                                         const MCRegisterInfo &MRI,
                                         const Triple &TheTriple, StringRef CPU,
                                         const MCTargetOptions &Options) {
  return new ELFZ80AsmBackend(T);
}
