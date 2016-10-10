//===-- Z80InstrInfo.cpp - Z80 Instruction Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Z80 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "Z80InstrInfo.h"
#include "Z80.h"
#include "Z80Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
using namespace llvm;

#define DEBUG_TYPE "z80-instr-info"

#define GET_INSTRINFO_CTOR_DTOR
#include "Z80GenInstrInfo.inc"

// Pin the vtable to this file.
void Z80InstrInfo::anchor() {}

Z80InstrInfo::Z80InstrInfo(Z80Subtarget &STI)
    : Z80GenInstrInfo((STI.is24Bit() ? Z80::ADJCALLSTACKDOWN24
                                     : Z80::ADJCALLSTACKDOWN16),
                      (STI.is24Bit() ? Z80::ADJCALLSTACKUP24
                                     : Z80::ADJCALLSTACKUP16)),
      Subtarget(STI), RI(STI.getTargetTriple()) {
}

/// Return the inverse of the specified condition,
/// e.g. turning COND_E to COND_NE.
Z80::CondCode Z80::GetOppositeBranchCondition(Z80::CondCode CC) {
  return Z80::CondCode(CC ^ 1);
}

bool Z80InstrInfo::isUnpredicatedTerminator(const MachineInstr &MI) const {
  if (!MI.isTerminator()) return false;

  // Conditional branch is a special case.
  if (MI.isBranch() && !MI.isBarrier())
    return true;
  if (!MI.isPredicable())
    return true;
  return !isPredicated(MI);
}

bool Z80InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                 MachineBasicBlock *&TBB,
                                 MachineBasicBlock *&FBB,
                                 SmallVectorImpl<MachineOperand> &Cond,
                                 bool AllowModify) const {
  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end(), UnCondBrIter = I;
  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;

    // Working from the bottom, when we see a non-terminator instruction, we're
    // done.
    if (!isUnpredicatedTerminator(*I))
      break;

    // A terminator that isn't a branch can't easily be handled by this
    // analysis.
    if (!I->isBranch())
      return true;

    // Cannot handle indirect branches.
    if (I->getOpcode() == Z80::JPr)
      return true;

    // Handle unconditional branches.
    if (I->getOpcode() == Z80::JQ) {
      UnCondBrIter = I;

      if (!AllowModify) {
        TBB = I->getOperand(0).getMBB();
        continue;
      }

      // If the block has any instructions after a JMP, delete them.
      while (std::next(I) != MBB.end())
        std::next(I)->eraseFromParent();
      Cond.clear();
      FBB = nullptr;

      // Delete the JMP if it's equivalent to a fall-through.
      if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
        TBB = nullptr;
        I->eraseFromParent();
        I = MBB.end();
        UnCondBrIter = I;
        continue;
      }

      // TBB is used to indicate the unconditional destination.
      TBB = I->getOperand(0).getMBB();
      continue;
    }

    // Handle conditional branches.
    assert(I->getOpcode() == Z80::JQCC && "Invalid conditional branch");
    Z80::CondCode BranchCode = Z80::CondCode(I->getOperand(1).getImm());

    // Working from the bottom, handle the first conditional branch.
    if (Cond.empty()) {
      MachineBasicBlock *TargetBB = I->getOperand(0).getMBB();
      if (AllowModify && UnCondBrIter != MBB.end() &&
          MBB.isLayoutSuccessor(TargetBB)) {
        // If we can modify the code and it ends in something like:
        //
        //     jCC L1
        //     jmp L2
        //   L1:
        //     ...
        //   L2:
        //
        // Then we can change this to:
        //
        //     jnCC L2
        //   L1:
        //     ...
        //   L2:
        //
        // Which is a bit more efficient.
        // We conditionally jump to the fall-through block.
        BranchCode = GetOppositeBranchCondition(BranchCode);
        MachineBasicBlock::iterator OldInst = I;

        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(Z80::JQCC))
          .addMBB(UnCondBrIter->getOperand(0).getMBB()).addImm(BranchCode);
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(Z80::JQ))
          .addMBB(TargetBB);

        OldInst->eraseFromParent();
        UnCondBrIter->eraseFromParent();

        // Restart the analysis.
        UnCondBrIter = MBB.end();
        I = MBB.end();
        continue;
      }

      FBB = TBB;
      TBB = I->getOperand(0).getMBB();
      Cond.push_back(MachineOperand::CreateImm(BranchCode));
      continue;
    }

    return true;
  }

  return false;
}

unsigned Z80InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                    int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;
    if (I->getOpcode() != Z80::JQ &&
        I->getOpcode() != Z80::JQCC &&
        I->getOpcode() != Z80::JPr)
      break;
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }

  return Count;
}

unsigned Z80InstrInfo::insertBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *TBB,
                                    MachineBasicBlock *FBB,
                                    ArrayRef<MachineOperand> Cond,
                                    const DebugLoc &DL,
                                    int *BytesAdded) const {
  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "Z80 branch conditions have one component!");
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
    // Unconditional branch?
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(Z80::JQ)).addMBB(TBB);
    return 1;
  }

  // Conditional branch.
  unsigned Count = 0;
  BuildMI(&MBB, DL, get(Z80::JQCC)).addMBB(TBB).addImm(Cond[0].getImm());
  ++Count;

  // If FBB is null, it is implied to be a fall-through block.
  if (!FBB) {
    // Two-way Conditional branch. Insert the second branch.
    BuildMI(&MBB, DL, get(Z80::JQ)).addMBB(FBB);
    ++Count;
  }
  return Count;
}

void Z80InstrInfo::getUnconditionalBranch(MCInst &Branch,
                                          const MCSymbolRefExpr *Target) const {
  Branch.setOpcode(Z80::JQ);
  Branch.addOperand(MCOperand::createExpr(Target));
}

void Z80InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MI,
                               const DebugLoc &DL, unsigned DstReg,
                               unsigned SrcReg, bool KillSrc) const {
  DEBUG(dbgs() << RI.getName(DstReg) << " = "
               << RI.getName(SrcReg) << '\n');
  for (auto Regs : {std::make_pair(DstReg, &SrcReg),
                    std::make_pair(SrcReg, &DstReg)}) {
    if (Z80::R8RegClass.contains(Regs.first) &&
        (Z80::R16RegClass.contains(*Regs.second) ||
         Z80::R24RegClass.contains(*Regs.second) ||
         Z80::R32RegClass.contains(*Regs.second)))
      *Regs.second = RI.getSubReg(*Regs.second, Z80::sub_low);
    else if (Z80::R24RegClass.contains(Regs.first) &&
             Z80::R32RegClass.contains(*Regs.second))
      *Regs.second = RI.getSubReg(*Regs.second, Z80::sub_long);
  }
  if (DstReg == SrcReg)
    return;
  if (Z80::G8RegClass.contains(DstReg, SrcReg)) {
    BuildMI(MBB, MI, DL, get(Z80::LD8rr), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  } else if (Z80::X8RegClass.contains(DstReg, SrcReg)) {
    BuildMI(MBB, MI, DL, get(Z80::LD8xx), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  } else if (Z80::Y8RegClass.contains(DstReg, SrcReg)) {
    BuildMI(MBB, MI, DL, get(Z80::LD8yy), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  } else if (Z80::R8RegClass.contains(DstReg, SrcReg)) {
    for (auto Reg : {&DstReg, &SrcReg}) {
      switch (*Reg) {
        case Z80::H: *Reg = Z80::D; break;
        case Z80::L: *Reg = Z80::E; break;
      }
    }
    unsigned EX = Subtarget.is24Bit() ? Z80::EX24DE : Z80::EX16DE;
    BuildMI(MBB, MI, DL, get(EX));
    BuildMI(MBB, MI, DL, get(Z80::LD8rr), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    BuildMI(MBB, MI, DL, get(EX));
    return;
  }
  bool Is24Bit = Z80::R24RegClass.contains(DstReg, SrcReg);
  if (KillSrc && (Is24Bit == Subtarget.is24Bit())) {
    bool DE = false, HL = false;
    for (auto Reg : {DstReg, SrcReg}) {
      switch (Reg) {
        case Z80::DE: case Z80::UDE: DE = true; break;
        case Z80::HL: case Z80::UHL: HL = true; break;
      }
    }
    if (DE && HL) {
      BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
      return;
    }
  }
  if ((SrcReg == Z80::SPS || SrcReg == Z80::SPL) &&
      (Z80::A16RegClass.contains(DstReg) ||
       Z80::A24RegClass.contains(DstReg))) {
    BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::LD24ri : Z80::LD16ri), DstReg)
      .addImm(0);
    BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao), DstReg)
      .addReg(DstReg).addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  if (Is24Bit) {
    // Both are 24 bits so preserve upper byte
    BuildMI(MBB, MI, DL, get(Z80::PUSH24r))
      .addReg(SrcReg, getKillRegState(KillSrc));
    BuildMI(MBB, MI, DL, get(Z80::POP24r), DstReg);
    return;
  }
  unsigned SubLo, SubHi;
  if (Z80::R32RegClass.contains(DstReg, SrcReg)) {
    SubLo = Z80::sub_long;
    SubHi = Z80::sub_top;
  } else {
    SubLo = Z80::sub_low;
    SubHi = Z80::sub_high;
  }
  unsigned DstLoReg = RI.getSubReg(DstReg, SubLo);
  unsigned SrcLoReg = RI.getSubReg(SrcReg, SubLo);
  unsigned DstHiReg = RI.getSubReg(DstReg, SubHi);
  unsigned SrcHiReg = RI.getSubReg(SrcReg, SubHi);
  if (DstHiReg && SrcHiReg && DstLoReg && SrcLoReg) {
    copyPhysReg(MBB, MI, DL, DstLoReg, SrcLoReg, KillSrc);
    copyPhysReg(MBB, MI, DL, DstHiReg, SrcHiReg, KillSrc);
    return;
  }
  llvm_unreachable("Unimplemented reg copy");
}

void Z80InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MI,
                                       unsigned SrcReg, bool isKill, int FI,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo *TRI) const {
  const MachineFunction &MF = *MBB.getParent();
  unsigned Opc;
  switch (RC->getSize()) {
  default: llvm_unreachable("Cannot store this register to stack slot!");
  case 1: Opc = Z80::LD8or;  break;
  case 2: Opc = Z80::LD16or; break;
  case 3: Opc = Z80::LD24or; break;
  }
  BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(Opc))
    .addFrameIndex(FI).addImm(0).addReg(SrcReg, getKillRegState(isKill));
}

void Z80InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        unsigned DstReg, int FI,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo *TRI) const {
  const MachineFunction &MF = *MBB.getParent();
  unsigned Opc;
  switch (RC->getSize()) {
  default: llvm_unreachable("Cannot load this register from stack slot!");
  case 1: Opc = Z80::LD8ro;  break;
  case 2: Opc = Z80::LD16ro; break;
  case 3: Opc = Z80::LD24ro; break;
  }
  BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(Opc), DstReg)
    .addFrameIndex(FI).addImm(0);
}

bool Z80InstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  MachineInstrBuilder MIB(*MI.getParent()->getParent(), MI);
  switch (MI.getOpcode()) {
  default: return false;
  case Z80::RCF:
    MIB->setDesc(get(Z80::OR8ar));
    MIB.addReg(Z80::A, RegState::Undef);
    DEBUG(MIB->dump());
    return true;
  case Z80::CALL16r:
  case Z80::CALL24r: {
    const char *symbol;
    switch (MIB->getOperand(0).getReg()) {
    default: llvm_unreachable("Unexpected indcall register");
    case Z80::HL: case Z80::UHL: symbol = "_indcallhl"; break;
    case Z80::IX: case Z80::UIX: symbol = "_indcallix"; break;
    case Z80::IY: case Z80::UIY: symbol = "_indcalliy"; break;
    }
    MIB->setDesc(get(MI.getOpcode() == Z80::CALL24r ? Z80::CALL24i
                                                    : Z80::CALL16i));
    MIB->getOperand(0).ChangeToES(symbol);
    DEBUG(MIB->dump());
    return true;
  }
  case Z80::TCRETURN16i:
  case Z80::TCRETURN24i:
    MIB->setDesc(get(Z80::JQ));
    DEBUG(MIB->dump());
    return true;
  case Z80::TCRETURN16r:
  case Z80::TCRETURN24r:
    MIB->setDesc(get(Z80::JPr));
    DEBUG(MIB->dump());
    return true;
  }
}

bool Z80InstrInfo::analyzeCompare(const MachineInstr &MI,
                                  unsigned &SrcReg, unsigned &SrcReg2,
                                  int &CmpMask, int &CmpValue) const {
  switch (MI.getOpcode()) {
  default: return false;
  case Z80::CP8ai:
    SrcReg = Z80::A;
    SrcReg2 = 0;
    CmpMask = ~0;
    CmpValue = MI.getOperand(0).getImm();
    return true;
  case Z80::CP8ar:
    SrcReg = Z80::A;
    SrcReg2 = MI.getOperand(0).getReg();
    CmpMask = ~0;
    CmpValue = 0;
    return true;
  case Z80::CP8am:
  case Z80::CP8ao:
    SrcReg = Z80::A;
    SrcReg2 = 0;
    CmpMask = ~0;
    CmpValue = 0;
    return true;
  }
}

bool Z80InstrInfo::optimizeCompareInstr(MachineInstr &CmpInstr,
                                        unsigned SrcReg, unsigned SrcReg2,
                                        int CmpMask, int CmpValue,
                                        const MachineRegisterInfo *MRI) const {
  // Get the unique definition of SrcReg.
  MachineInstr *MI = MRI->getUniqueVRegDef(SrcReg);
  if (!MI) return false;

  // CmpInstr is the first instruction of the BB.
  MachineBasicBlock::iterator I = CmpInstr, Def = MI;

  // If we are comparing against zero, check whether we can use MI to update F.
  // If MI is not in the same BB as CmpInstr, do not optimize.
  bool IsCmpZero = (SrcReg2 == 0 && CmpValue == 0);
  if (IsCmpZero && MI->getParent() != CmpInstr.getParent())
    return false;

  llvm_unreachable("Unimplemented!");
}
