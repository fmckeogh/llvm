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
#include "llvm/Support/ErrorHandling.h"
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
void Z80InstPrinterBase::printCCOperand(const MCInst *MI, unsigned Op,
                                        raw_ostream &OS) {
  switch (MI->getOperand(Op).getImm()) {
  default: llvm_unreachable("Invalid CC operand!");
    case 0: OS << "nz"; break;
    case 1: OS << "z"; break;
    case 2: OS << "nc"; break;
    case 3: OS << "c"; break;
    case 4: OS << "po"; break;
    case 5: OS << "pe"; break;
    case 6: OS << "p"; break;
    case 7: OS << "m"; break;
  }
}

void Z80InstPrinterBase::printMem(const MCInst *MI, unsigned Op,
                                  raw_ostream &OS) {
  OS << markup("<mem:") << '(';
  printOperand(MI, Op, OS);
  OS << ')' << markup(">");;
}
void Z80InstPrinterBase::printPtr(const MCInst *MI, unsigned Op,
                                  raw_ostream &OS) {
  OS << markup("<mem:") << '(';
  printOperand(MI, Op, OS);
  OS << ')' << markup(">");
}
void Z80InstPrinterBase::printOff(const MCInst *MI, unsigned Op,
                                   raw_ostream &OS) {
  OS << markup("<mem:") << '(';
  printAddr(MI, Op, OS);
  OS << ')' << markup(">");
}
void Z80InstPrinterBase::printAddr(const MCInst *MI, unsigned Op,
                                   raw_ostream &OS) {
  printOperand(MI, Op, OS);
  int8_t Off = MI->getOperand(Op+1).getImm();
  assert(Off == MI->getOperand(Op+1).getImm() && "Offset out of range!");
  OS << " + " << int(MI->getOperand(Op+1).getImm());
}
