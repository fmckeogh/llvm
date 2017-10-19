//===-- Z80AsmParser.cpp - Parse Z80 assembly to MCInst instructions ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "Z80Operand.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace {

class Z80AsmParser : public MCTargetAsmParser {
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
      : MCTargetAsmParser(Options, sti, mii), InstInfo(nullptr) {}

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;
};

} // end anonymous namespace

bool Z80AsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                           OperandVector &Operands,
                                           MCStreamer &Out, uint64_t &ErrorInfo,
                                           bool MatchingInlineAsm) {
  llvm_unreachable("Unimplemented");
}

bool Z80AsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                 SMLoc &EndLoc) {
  llvm_unreachable("Unimplemented");
}

bool Z80AsmParser::ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                                    SMLoc NameLoc, OperandVector &Operands) {
  llvm_unreachable("Unimplemented");
}

bool Z80AsmParser::ParseDirective(AsmToken DirectiveID) {
  llvm_unreachable("Unimplemented");
}

// Force static initialization.
extern "C" void LLVMInitializeZ80AsmParser() {
  RegisterMCAsmParser<Z80AsmParser> X(getTheZ80Target());
  RegisterMCAsmParser<Z80AsmParser> Y(getTheEZ80Target());
}

namespace {
struct MatchEntry;
}
namespace std {
inline const MatchEntry *begin(const MatchEntry array[]) { return array; }
inline const MatchEntry *end(const MatchEntry array[]) { return array; }
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#define GET_SUBTARGET_FEATURE_NAME
#include "Z80GenAsmMatcher.inc"
