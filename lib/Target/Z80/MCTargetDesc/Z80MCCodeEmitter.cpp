//===-- X86MCCodeEmitter.cpp - Convert X86 code to machine code -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the X86MCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "Z80FixupKinds.h"
#include "Z80MCTargetDesc.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

namespace {

class Z80MCCodeEmitter : public MCCodeEmitter {
  Z80MCCodeEmitter(const Z80MCCodeEmitter &) = delete;
  void operator=(const Z80MCCodeEmitter &) = delete;
  const MCInstrInfo &MII;
  MCContext &Ctx;
public:
  Z80MCCodeEmitter(const MCInstrInfo &mii, MCContext &ctx)
    : MII(mii), Ctx(ctx) {}

  ~Z80MCCodeEmitter() override {}

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;
};

} // end anonymous namespace

void Z80MCCodeEmitter::encodeInstruction(const MCInst &MC, raw_ostream &OS,
                                         SmallVectorImpl<MCFixup> &Fixups,
                                         const MCSubtargetInfo &STI) const {
  llvm_unreachable("Unimplemented");
}

MCCodeEmitter *llvm::createZ80MCCodeEmitter(const MCInstrInfo &MII,
                                            const MCRegisterInfo &MRI,
                                            MCContext &Ctx) {
  return new Z80MCCodeEmitter(MII, Ctx);
}
