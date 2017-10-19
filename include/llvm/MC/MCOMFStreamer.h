//===- MCOMFStreamer.h - MCStreamer OMF Object File Interface ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCOMFSTREAMER_H
#define LLVM_MC_MCOMFSTREAMER_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCObjectStreamer.h"

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCExpr;
class MCInst;

class MCOMFStreamer : public MCObjectStreamer {
public:
  MCOMFStreamer(MCContext &Context, std::unique_ptr<MCAsmBackend> TAB,
                raw_pwrite_stream &OS, std::unique_ptr<MCCodeEmitter> Emitter)
      : MCObjectStreamer(Context, std::move(TAB), OS, std::move(Emitter)) {}

  ~MCOMFStreamer() override = default;

  bool EmitSymbolAttribute(MCSymbol *Symbol, MCSymbolAttr Attribute) override;
  void EmitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                        unsigned ByteAlignment) override;
  void EmitZerofill(MCSection *Section, MCSymbol *Symbol = nullptr,
                    uint64_t Size = 0, unsigned ByteAlignment = 0) override;
  void EmitInstToData(const MCInst &Inst, const MCSubtargetInfo &) override;
};



MCOMFStreamer *createARMOMFStreamer(MCContext &Context, MCAsmBackend &TAB,
                                    raw_pwrite_stream &OS,
                                    MCCodeEmitter *Emitter, bool RelaxAll,
                                    bool IsThumb);

} // end namespace llvm

#endif // LLVM_MC_MCOMFSTREAMER_H
