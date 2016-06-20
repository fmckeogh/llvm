//===- Z80InstPrinter.cpp - Convert Z80 MCInst to assembly ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints a Z80 MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "Z80InstPrinterBase.h"
#include "Z80InstPrinter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

void Z80InstPrinterBase::printRegName(raw_ostream &OS, unsigned RegNo) const {
  OS << markup("<reg:") << getRegName(RegNo) << markup(">");
}

void Z80InstPrinterBase::printInst(const MCInst *MI, raw_ostream &OS,
                               StringRef Annot, const MCSubtargetInfo &STI) {
  printInstruction(MI, OS);
  printAnnotation(OS, Annot);
}

void Z80InstPrinterBase::printOperand(const MCInst *MI, unsigned OpNo,
                                  raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    printRegName(OS, Op.getReg());
  } else if (Op.isImm()) {
    OS << Op.getImm();
  } else {
    assert(Op.isExpr() && "unknown operand kind in printOperand");
    OS << markup("<imm:");
    Op.getExpr()->print(OS, &MAI);
    OS << markup(">");
  }
}

void Z80InstPrinterBase::printImmMem(const MCInst *MI, unsigned Op,
                                 raw_ostream &OS) {
  OS << markup("<mem:") << '(';
  MI->getOperand(Op).getExpr()->print(OS, &MAI);
  OS << ')' << markup(">");;
}
void Z80InstPrinterBase::printRegOffAddr(const MCInst *MI, unsigned Op,
                                     raw_ostream &OS) {
  printOperand(MI, Op, OS);
  if (auto off = MI->getOperand(Op+1).getImm()) {
    if (off >= 0) {
      OS << " + " <<  off;
    } else {
      OS << " - " << -off;
    }
  }
}
void Z80InstPrinterBase::printRegOffMem(const MCInst *MI, unsigned Op,
                                    raw_ostream &OS) {
  OS << markup("<mem:") << '(';
  printRegOffAddr(MI, Op, OS);
  OS << ')' << markup(">");
}
