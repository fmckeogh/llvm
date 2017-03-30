//===-- Z80AsmPrinter.cpp - Convert Z80 LLVM code to AT&T assembly --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to Z80 machine code.
//
//===----------------------------------------------------------------------===//

#include "Z80AsmPrinter.h"
#include "Z80.h"
#include "MCTargetDesc/Z80TargetStreamer.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
// Target Registry Stuff
//===----------------------------------------------------------------------===//

void Z80AsmPrinter::EmitStartOfAsmFile(Module &M) {
 const Triple &TT = TM.getTargetTriple();
 bool is16 = TT.isArch16Bit() || TT.getEnvironment() == Triple::CODE16;
 if (M.getModuleInlineAsm().empty())
   OutStreamer->EmitAssemblerFlag(is16 ? MCAF_Code16 : MCAF_Code24);
}

void Z80AsmPrinter::EmitEndOfAsmFile(Module &M) {
  Z80TargetStreamer *TS =
    static_cast<Z80TargetStreamer *>(OutStreamer->getTargetStreamer());
  for (const auto &Symbol : OutContext.getSymbols())
    if (!Symbol.second->isDefined())
      TS->emitExtern(Symbol.second);
  TS->emitEnd();
}

// Force static initialization.
extern "C" void LLVMInitializeZ80AsmPrinter() {
  RegisterAsmPrinter<Z80AsmPrinter> X(TheZ80Target);
  RegisterAsmPrinter<Z80AsmPrinter> Y(TheEZ80Target);
}
