//===--- Z80Operand.h - Parsed Z80 machine instruction --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_ASMPARSER_Z80OPERAND_H
#define LLVM_LIB_TARGET_Z80_ASMPARSER_Z80OPERAND_H

#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/Support/Error.h"

namespace llvm {

/// Z80Operand - Instances of this class represent a parsed Z80 machine operand.
struct Z80Operand : public MCParsedAsmOperand {
  StringRef getToken() const {
    llvm_unreachable("Unimplemented");
  }
  void addRegOperands(MCInst &Inst, unsigned N) const {
    llvm_unreachable("Unimplemented");
  }
};

}

#endif
