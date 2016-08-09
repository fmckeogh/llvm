//===-- Z80AsmParser.cpp - Parse Z80 assembly to MCInst instructions ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace {

class Z80AsmParser : public MCTargetAsmParser {
  const MCInstrInfo &MII;
  ParseInstructionInfo *InstInfo;

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  /// @name Auto-generated Matcher Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "Z80GenAsmMatcher.inc"

  /// }

public:
  Z80AsmParser(const MCSubtargetInfo &sti, MCAsmParser &Parser,
               const MCInstrInfo &mii, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, sti), MII(mii), InstInfo(nullptr) {}

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;
};

} // end anonymous namespace
// Force static initialization.
extern "C" void LLVMInitializeZ80AsmParser() {
  RegisterMCAsmParser<Z80AsmParser> X(TheZ80Target);
  RegisterMCAsmParser<Z80AsmParser> Y(TheEZ80Target);
}
