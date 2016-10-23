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

bool Z80::splitReg(
    unsigned ByteSize, unsigned Opc8, unsigned Opc16, unsigned Opc24,
    unsigned &RC, unsigned &LoOpc, unsigned &LoIdx,
    unsigned &HiOpc, unsigned &HiIdx, unsigned &HiOff,
    const Z80Subtarget &Subtarget) {
  switch (ByteSize) {
  default: llvm_unreachable("Unknown Size!");
  case 1:
    RC = Z80::R8RegClassID;
    LoOpc = HiOpc = Opc8;
    LoIdx = HiIdx = Z80::NoSubRegister;
    HiOff = 0;
    return false;
  case 2:
    RC = Z80::R16RegClassID;
    if (Subtarget.hasEZ80Ops()) {
      LoOpc = HiOpc = Opc16;
      LoIdx = HiIdx = Z80::NoSubRegister;
      HiOff = 0;
      return false;
    }
    LoOpc = HiOpc = Opc8;
    LoIdx = Z80::sub_low;
    HiIdx = Z80::sub_high;
    HiOff = 1;
    return true;
  case 3:
    // Legalization should have taken care of this if we don't have eZ80 ops
    assert(Subtarget.hasEZ80Ops() && "Need eZ80 word load/store");
    RC = Z80::R24RegClassID;
    LoOpc = HiOpc = Opc24;
    LoIdx = HiIdx = Z80::NoSubRegister;
    HiOff = 0;
    return false;
  case 4:
    // Legalization should have taken care of this if we don't have eZ80 ops
    assert(Subtarget.hasEZ80Ops() && "Need eZ80 word load/store");
    RC = Z80::R32RegClassID;
    LoOpc = Opc24;
    HiOpc = Opc8;
    LoIdx = Z80::sub_long;
    HiIdx = Z80::sub_top;
    HiOff = 3;
    return true;
  }
}

bool Z80InstrInfo::canExchange(unsigned RegA, unsigned RegB) const {
  // The only regs that can be directly exchanged are DE and HL, in any order.
  bool DE = false, HL = false;
  for (unsigned Reg : {RegA, RegB}) {
    if (RI.isSubRegisterEq(Z80::UDE, Reg))
      DE = true;
    else if (RI.isSubRegisterEq(Z80::UHL, Reg))
      HL = true;
  }
  return DE && HL;
}

void Z80InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MI,
                               const DebugLoc &DL, unsigned DstReg,
                               unsigned SrcReg, bool KillSrc) const {
  DEBUG(dbgs() << RI.getName(DstReg) << " = " << RI.getName(SrcReg) << '\n');
  /*for (auto Regs : {std::make_pair(DstReg, &SrcReg),
                    std::make_pair(SrcReg, &DstReg)}) {
    if (Z80::R8RegClass.contains(Regs.first) &&
        (Z80::R16RegClass.contains(*Regs.second) ||
         Z80::R24RegClass.contains(*Regs.second) ||
         Z80::R32RegClass.contains(*Regs.second)))
      *Regs.second = RI.getSubReg(*Regs.second, Z80::sub_low);
    else if (Z80::R24RegClass.contains(Regs.first) &&
             Z80::R32RegClass.contains(*Regs.second))
      *Regs.second = RI.getSubReg(*Regs.second, Z80::sub_long);
  }*/
  // Identity copy.
  if (DstReg == SrcReg)
    return;
  if (Z80::R8RegClass.contains(DstReg, SrcReg)) {
    // Byte copy.
    if (Z80::G8RegClass.contains(DstReg, SrcReg)) {
      // Neither are index registers.
      BuildMI(MBB, MI, DL, get(Z80::LD8rr), DstReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (Z80::I8RegClass.contains(DstReg, SrcReg)) {
      // Both are index registers.
      if (Z80::X8RegClass.contains(DstReg, SrcReg)) {
        BuildMI(MBB, MI, DL, get(Z80::LD8xx), DstReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
      } else if (Z80::Y8RegClass.contains(DstReg, SrcReg)) {
        BuildMI(MBB, MI, DL, get(Z80::LD8yy), DstReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
      } else {
        // We are copying between different index registers, so we need to use
        // an intermediate register.
        BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::PUSH24r
                                                     : Z80::PUSH16r))
          .addReg(Z80::AF);
        BuildMI(MBB, MI, DL, get(Z80::X8RegClass.contains(SrcReg) ? Z80::LD8xx
                                                                  : Z80::LD8yy),
                Z80::A).addReg(SrcReg, getKillRegState(KillSrc));
        BuildMI(MBB, MI, DL, get(Z80::X8RegClass.contains(DstReg) ? Z80::LD8xx
                                                                  : Z80::LD8yy),
                DstReg).addReg(Z80::A);
        BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::POP24r
                                                     : Z80::POP16r), Z80::AF);
      }
    } else {
      // Only one is an index register, which isn't directly possible if one of
      // them is from HL.  If so, surround with EX DE,HL and use DE instead.
      bool NeedEX = false;
      for (unsigned *Reg : {&DstReg, &SrcReg}) {
        switch (*Reg) {
        case Z80::H: *Reg = Z80::D; NeedEX = true; break;
        case Z80::L: *Reg = Z80::E; NeedEX = true; break;
        }
      }
      auto EX = get(Subtarget.is24Bit() ? Z80::EX24DE : Z80::EX16DE);
      if (NeedEX)
        BuildMI(MBB, MI, DL, EX);
      BuildMI(MBB, MI, DL, get(Z80::X8RegClass.contains(DstReg) ||
                               Z80::X8RegClass.contains(SrcReg) ? Z80::LD8xx
                                                                : Z80::LD8yy),
              DstReg).addReg(SrcReg, getKillRegState(KillSrc));
      if (NeedEX)
        BuildMI(MBB, MI, DL, EX);
    }
    return;
  }
  // Specialized word copy.
  bool Is24Bit = Z80::R24RegClass.contains(DstReg, SrcReg);
  // Special case DE/HL = HL/DE<kill> as EX DE,HL.
  if (KillSrc && Is24Bit == Subtarget.is24Bit() &&
      canExchange(DstReg, SrcReg)) {
    BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
      .addReg(DstReg, RegState::ImplicitDefine)
      .addReg(SrcReg, RegState::ImplicitKill);
    return;
  }
  // Copies to SP.
  if (DstReg == Z80::SPS || DstReg == Z80::SPL) {
    assert((Z80::A16RegClass.contains(SrcReg) || SrcReg == Z80::DE ||
            Z80::A24RegClass.contains(SrcReg) || SrcReg == Z80::UDE) &&
           "Unimplemented");
    if (SrcReg == Z80::DE || SrcReg == Z80::UDE)
      BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
    BuildMI(MBB, MI, DL, get(DstReg == Z80::SPL ? Z80::LD24SP : Z80::LD16SP))
      .addReg(SrcReg, getKillRegState(KillSrc));
    if (SrcReg == Z80::DE || SrcReg == Z80::UDE)
      BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
    return;
  }
  // Copies from SP.
  if (SrcReg == Z80::SPS || SrcReg == Z80::SPL) {
    assert((Z80::A16RegClass.contains(DstReg) || DstReg == Z80::DE ||
            Z80::A24RegClass.contains(DstReg) || DstReg == Z80::UDE) &&
           "Unimplemented");
    if (DstReg == Z80::DE || DstReg == Z80::UDE)
      BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
    BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::LD24ri : Z80::LD16ri),
            DstReg).addImm(0);
    BuildMI(MBB, MI, DL, get(SrcReg == Z80::SPL ? Z80::ADD24ao : Z80::ADD16ao),
            DstReg).addReg(DstReg).addReg(SrcReg, getKillRegState(KillSrc));
    if (DstReg == Z80::DE || DstReg == Z80::UDE)
      BuildMI(MBB, MI, DL, get(Is24Bit ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
    return;
  }
  // If both are 24-bit then the upper byte needs to be preserved.
  if (Is24Bit) {
    BuildMI(MBB, MI, DL, get(Z80::PUSH24r))
      .addReg(SrcReg, getKillRegState(KillSrc));
    BuildMI(MBB, MI, DL, get(Z80::POP24r), DstReg);
    return;
  }
  // Otherwise, implement as two copies. A 16-bit copy should copy high and low
  // 8 bits separately. A 32-bit copy should copy high 8 bits and low 24 bits.
  bool Is32Bit = Z80::R32RegClass.contains(DstReg, SrcReg);
  assert((Is32Bit || Z80::R16RegClass.contains(DstReg, SrcReg)) &&
         "Unknown copy width");
  unsigned SubLo = Is32Bit ? Z80::sub_long : Z80::sub_low;
  unsigned SubHi = Is32Bit ? Z80::sub_top : Z80::sub_high;
  unsigned DstLoReg = RI.getSubReg(DstReg, SubLo);
  unsigned SrcLoReg = RI.getSubReg(SrcReg, SubLo);
  unsigned DstHiReg = RI.getSubReg(DstReg, SubHi);
  unsigned SrcHiReg = RI.getSubReg(SrcReg, SubHi);
  bool DstLoSrcHiOverlap = RI.regsOverlap(DstLoReg, SrcHiReg);
  bool SrcLoDstHiOverlap = RI.regsOverlap(SrcLoReg, DstHiReg);
  if (DstLoSrcHiOverlap && SrcLoDstHiOverlap) {
    assert(KillSrc &&
           "Both parts of SrcReg and DstReg overlap but not killing source!");
    // e.g. EUHL = LUDE so just swap the operands
    unsigned OtherReg;
    if (canExchange(DstLoReg, SrcLoReg)) {
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::EX24DE : Z80::EX16DE))
        .addReg(DstReg, RegState::ImplicitDefine)
        .addReg(SrcReg, RegState::ImplicitKill);
    } else if ((OtherReg = DstLoReg, RI.isSubRegisterEq(Z80::UHL, SrcLoReg)) ||
               (OtherReg = SrcLoReg, RI.isSubRegisterEq(Z80::UHL, DstLoReg))) {
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::PUSH24r : Z80::PUSH16r))
        .addReg(OtherReg, RegState::Kill);
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::EX24SP : Z80::EX16SP));
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::POP24r : Z80::POP16r),
              OtherReg);
    } else {
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::PUSH24r : Z80::PUSH16r))
        .addReg(SrcLoReg, RegState::Kill);
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::PUSH24r : Z80::PUSH16r))
        .addReg(DstLoReg, RegState::Kill);
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::POP24r : Z80::POP16r),
              SrcLoReg);
      BuildMI(MBB, MI, DL, get(Subtarget.is24Bit() ? Z80::POP24r : Z80::POP16r),
              DstLoReg);
    }
    // Check if top needs to be moved (e.g. EUHL = HUDE).
    unsigned DstHiIdx = RI.getSubRegIndex(SrcLoReg, DstHiReg);
    unsigned SrcHiIdx = RI.getSubRegIndex(DstLoReg, SrcHiReg);
    if (DstHiIdx != SrcHiIdx)
      copyPhysReg(MBB, MI, DL, DstHiReg,
                  RI.getSubReg(DstLoReg, SrcHiIdx), KillSrc);
  } else if (DstLoSrcHiOverlap) {
    // Copy out SrcHi before SrcLo overwrites it.
    copyPhysReg(MBB, MI, DL, DstHiReg, SrcHiReg, KillSrc);
    copyPhysReg(MBB, MI, DL, DstLoReg, SrcLoReg, KillSrc);
  } else {
    // If SrcLoDstHiOverlap then copy out SrcLo before SrcHi overwrites it,
    // otherwise the order doesn't matter.
    copyPhysReg(MBB, MI, DL, DstLoReg, SrcLoReg, KillSrc);
    copyPhysReg(MBB, MI, DL, DstHiReg, SrcHiReg, KillSrc);
  }
  --MI;
  MI->addRegisterDefined(DstReg, &RI);
  if (KillSrc)
    MI->addRegisterKilled(SrcReg, &RI, true);
}

void Z80InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MI,
                                       unsigned SrcReg, bool IsKill, int FI,
                                       const TargetRegisterClass *TRC,
                                       const TargetRegisterInfo *TRI) const {
  unsigned RC, LoOpc, LoIdx, HiOpc, HiIdx, HiOff;
  if (Z80::splitReg(TRC->getSize(), Z80::LD8or, Z80::LD16or, Z80::LD24or,
                    RC, LoOpc, LoIdx, HiOpc, HiIdx, HiOff, Subtarget))
    BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(LoOpc))
      .addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(IsKill), LoIdx);
  MachineInstrBuilder MIB = BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(HiOpc))
    .addFrameIndex(FI).addImm(HiOff)
    .addReg(SrcReg, getKillRegState(IsKill), HiIdx);
  if (IsKill)
    MIB->addRegisterKilled(SrcReg, TRI, true);
}
void Z80InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        unsigned DstReg, int FI,
                                        const TargetRegisterClass *TRC,
                                        const TargetRegisterInfo *TRI) const {
  unsigned RC, LoOpc, LoIdx, HiOpc, HiIdx, HiOff;
  if (Z80::splitReg(TRC->getSize(), Z80::LD8ro, Z80::LD16ro, Z80::LD24ro,
                    RC, LoOpc, LoIdx, HiOpc, HiIdx, HiOff, Subtarget))
    BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(LoOpc)).addDef(DstReg, 0, LoIdx)
      .addFrameIndex(FI).addImm(0);
  BuildMI(MBB, MI, MBB.findDebugLoc(MI), get(HiOpc)).addDef(DstReg, 0, HiIdx)
    .addFrameIndex(FI).addImm(HiOff)->addRegisterDefined(DstReg, TRI);
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
