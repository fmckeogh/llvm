//===-- Z80ELFObjectWriter.cpp - Z80 ELF Writer ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Z80MCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
using namespace llvm;

namespace {
  class Z80ELFObjectWriter : public MCELFObjectTargetWriter {
  public:
    Z80ELFObjectWriter(uint8_t OSABI);

    ~Z80ELFObjectWriter() override;

  protected:
    unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                          const MCFixup &Fixup, bool IsPCRel) const override;
  };
}

Z80ELFObjectWriter::Z80ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*IsELF64*/ false, OSABI, ELF::EM_Z80,
                              /*HasRelocationAddend*/ false) {}

Z80ELFObjectWriter::~Z80ELFObjectWriter() {}

unsigned
Z80ELFObjectWriter::getRelocType(MCContext &Ctx, const MCValue &Target,
                                 const MCFixup &Fixup, bool IsPCRel) const {
  llvm_unreachable("Unimplemented");
}

MCObjectWriter *llvm::createZ80ELFObjectWriter(raw_pwrite_stream &OS,
                                               uint8_t OSABI) {
  MCELFObjectTargetWriter *MOTW = new Z80ELFObjectWriter(OSABI);
  return createELFObjectWriter(MOTW, OS, /*IsLittleEndian*/ true);
}
