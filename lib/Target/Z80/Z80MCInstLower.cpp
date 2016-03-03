//===-- Z80MCInstLower.cpp - Convert Z80 MachineInstr to an MCInst --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower Z80 MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "Z80AsmPrinter.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
using namespace llvm;

namespace {
  /// Z80MCInstLower - This class is used to lower a MachineInstr into an MCInst.
  class Z80MCInstLower {
    MCContext &Ctx;
    Z80AsmPrinter &Printer;

  public:
    Z80MCInstLower(MCContext &Ctx, Z80AsmPrinter &AP);

    Optional<MCOperand> LowerMachineOperand(const MachineInstr *MI,
                                            const MachineOperand &MO) const;

    void Lower(const MachineInstr *MI, MCInst &OutMI) const;
  };
}

Z80MCInstLower::Z80MCInstLower(MCContext &MCC,
                               Z80AsmPrinter &AP)
    : Ctx(MCC), Printer(AP) {}

Optional<MCOperand>
Z80MCInstLower::LowerMachineOperand(const MachineInstr *MI,
                                    const MachineOperand &MO) const {
  switch (MO.getType()) {
  default:
    MI->dump();
    llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    return MCOperand::createReg(MO.getReg());
  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm());
  }
}

void Z80MCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  MCOperand MCOp;
  for (const MachineOperand &MO : MI->operands())
    if (auto MaybeMCOp = LowerMachineOperand(MI, MO))
      OutMI.addOperand(MaybeMCOp.getValue());
}

void Z80AsmPrinter::EmitInstruction(const MachineInstr *MI) {
  Z80MCInstLower MCInstLowering(OutContext, *this);

  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);
  EmitToStreamer(*OutStreamer, TmpInst);
}
