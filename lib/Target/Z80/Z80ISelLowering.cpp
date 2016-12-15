//===-- Z80ISelLowering.cpp - Z80 DAG Lowering Implementation -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Z80 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "Z80ISelLowering.h"
#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "Z80Subtarget.h"
#include "Z80TargetMachine.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
using namespace llvm;

#define DEBUG_TYPE "z80-isel"

Z80TargetLowering::Z80TargetLowering(const Z80TargetMachine &TM,
                                     const Z80Subtarget &STI)
    : TargetLowering(TM), Subtarget(STI) {
  bool HasEZ80Ops = Subtarget.hasEZ80Ops(), Is24Bit = Subtarget.is24Bit();
  MVT PtrVT = MVT::getIntegerVT(8 * TM.getPointerSize());

  setSchedulingPreference(Sched::RegPressure);

  // Set up the register classes.
  addRegisterClass(MVT::i8, &Z80::R8RegClass);
  addRegisterClass(MVT::i16, &Z80::R16RegClass);
  if (Is24Bit) {
    addRegisterClass(MVT::i24, &Z80::R24RegClass);
    //addRegisterClass(MVT::i32, &Z80::R24RegClass);
  }
  for (auto VT : { MVT::i16, MVT::i24, MVT::i32 })
    for (unsigned Opc : { ISD::AND, ISD::OR, ISD::XOR,
                          ISD::SHL, ISD::SRA, ISD::SRL })
      setOperationAction(Opc, VT, LibCall);
  for (auto VT : { MVT::i8, MVT::i16, MVT::i24, MVT::i32 }) {
    for (unsigned Opc : { ISD::MUL,
                          ISD::SDIV,    ISD::UDIV,
                          ISD::SREM,    ISD::UREM,
                          ISD::SDIVREM, ISD::UDIVREM })
      setOperationAction(Opc, VT, LibCall);
    for (unsigned Opc : { ISD::SHL_PARTS, ISD::SRA_PARTS, ISD::SRL_PARTS,
                          ISD::SMUL_LOHI, ISD::UMUL_LOHI,
                          ISD::SMULO,     ISD::UMULO,
                          ISD::MULHU,     ISD::MULHS,
                          ISD::SELECT })
      setOperationAction(Opc, VT, Expand);
    for (unsigned Opc : { ISD::BR_CC, ISD::SETCC, ISD::SELECT_CC })
      setOperationAction(Opc, VT, Custom);
  }
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  if (HasEZ80Ops)
    setOperationAction(ISD::MUL, MVT::i8, Custom);

  if (!HasEZ80Ops)
    setOperationAction(ISD::LOAD, MVT::i16, Custom);
  if (!HasEZ80Ops || Is24Bit)
    setOperationAction(ISD::STORE, MVT::i16, Custom);
  if (Is24Bit) {
    //setOperationAction(ISD::LOAD, MVT::i32, Custom);
    //setOperationAction(ISD::STORE, MVT::i32, Custom);
  }
  for (MVT ValVT : MVT::integer_valuetypes()) {
    for (MVT MemVT : MVT::integer_valuetypes()) {
      setLoadExtAction(ISD:: EXTLOAD, ValVT, MemVT, Expand);
      setLoadExtAction(ISD::ZEXTLOAD, ValVT, MemVT, Expand);
      setLoadExtAction(ISD::SEXTLOAD, ValVT, MemVT, Expand);
      setTruncStoreAction(ValVT, MemVT, Expand);
    }
  }
  if (Is24Bit)
    setLoadExtAction(ISD::EXTLOAD, MVT::i24, MVT::i16, Custom);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, PtrVT, Expand);

  setStackPointerRegisterToSaveRestore(Is24Bit ? Z80::SPL : Z80::SPS);

  // Compute derived properties from the register classes
  computeRegisterProperties(STI.getRegisterInfo());

  setBooleanContents(ZeroOrOneBooleanContent);
  setJumpIsExpensive();

  setLibcallName(RTLIB::ZEXT_I16_I24, "_stoiu");
  setLibcallCallingConv(RTLIB::ZEXT_I16_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SEXT_I16_I24, "_stoi");
  setLibcallCallingConv(RTLIB::SEXT_I16_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SEXT_I24_I32, "_itol");
  setLibcallCallingConv(RTLIB::SEXT_I24_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NOT_I16, "_snot");
  setLibcallCallingConv(RTLIB::NOT_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NOT_I24, "_inot");
  setLibcallCallingConv(RTLIB::NOT_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NOT_I32, "_lnot");
  setLibcallCallingConv(RTLIB::NOT_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::AND_I16, "_sand");
  setLibcallCallingConv(RTLIB::AND_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::AND_I24, "_iand");
  setLibcallCallingConv(RTLIB::AND_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::AND_I32, "_land");
  setLibcallCallingConv(RTLIB::AND_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::OR_I16, "_sor");
  setLibcallCallingConv(RTLIB::OR_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::OR_I24, "_ior");
  setLibcallCallingConv(RTLIB::OR_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::OR_I32, "_lor");
  setLibcallCallingConv(RTLIB::OR_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::XOR_I16, "_sxor");
  setLibcallCallingConv(RTLIB::XOR_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::XOR_I24, "_ixor");
  setLibcallCallingConv(RTLIB::XOR_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::XOR_I32, "_lxor");
  setLibcallCallingConv(RTLIB::XOR_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I8, "_bshl");
  setLibcallCallingConv(RTLIB::SHL_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I16, "_sshl");
  setLibcallCallingConv(RTLIB::SHL_I16, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SHL_I16_I8, "_sshl_b");
  setLibcallCallingConv(RTLIB::SHL_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I24, "_ishl");
  setLibcallCallingConv(RTLIB::SHL_I24, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SHL_I24_I8, "_ishl_b");
  setLibcallCallingConv(RTLIB::SHL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I32, "_lshl");
  setLibcallCallingConv(RTLIB::SHL_I32, CallingConv::Z80_LibCall_L);
  setLibcallName(RTLIB::SRA_I8, "_bshrs");
  setLibcallCallingConv(RTLIB::SRA_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I16, "_sshrs");
  setLibcallCallingConv(RTLIB::SRA_I16, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SRA_I16_I8, "_sshrs_b");
  setLibcallCallingConv(RTLIB::SRA_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I24, "_ishrs");
  setLibcallCallingConv(RTLIB::SRA_I24, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SRA_I24_I8, "_ishrs_b");
  setLibcallCallingConv(RTLIB::SRA_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I32, "_lshrs");
  setLibcallCallingConv(RTLIB::SRA_I32, CallingConv::Z80_LibCall_L);
  setLibcallName(RTLIB::SRL_I8, "_bshl");
  setLibcallCallingConv(RTLIB::SRL_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I16, "_sshru");
  setLibcallCallingConv(RTLIB::SRL_I16, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SRL_I16_I8, "_sshru_b");
  setLibcallCallingConv(RTLIB::SRL_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I24, "_ishru");
  setLibcallCallingConv(RTLIB::SRL_I24, CallingConv::Z80_LibCall_C);
  setLibcallName(RTLIB::SRL_I24_I8, "_ishru_b");
  setLibcallCallingConv(RTLIB::SRL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I32, "_lshru");
  setLibcallCallingConv(RTLIB::SRL_I32, CallingConv::Z80_LibCall_L);
  setLibcallName(RTLIB::CMP_I32, "_lcmpu");
  setLibcallCallingConv(RTLIB::CMP_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::CMP_I16_0, "_scmpzero");
  setLibcallCallingConv(RTLIB::CMP_I16_0, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::CMP_I24_0, "_icmpzero");
  setLibcallCallingConv(RTLIB::CMP_I24_0, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::CMP_I32_0, "_lcmpzero");
  setLibcallCallingConv(RTLIB::CMP_I32_0, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SCMP, "_setflag");
  setLibcallCallingConv(RTLIB::SCMP, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NEG_I16, "_sneg");
  setLibcallCallingConv(RTLIB::NEG_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NEG_I24, "_ineg");
  setLibcallCallingConv(RTLIB::NEG_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::NEG_I32, "_lneg");
  setLibcallCallingConv(RTLIB::NEG_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::ADD_I32, "_ladd");
  setLibcallCallingConv(RTLIB::ADD_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::ADD_I32_I8, "_ladd_b");
  setLibcallCallingConv(RTLIB::ADD_I32_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SUB_I32, "_lsub");
  setLibcallCallingConv(RTLIB::SUB_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I8, "_bmulu");
  setLibcallCallingConv(RTLIB::MUL_I8, CallingConv::Z80_LibCall_BC);
  setLibcallName(RTLIB::MUL_I16, "_smulu");
  setLibcallCallingConv(RTLIB::MUL_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I24, "_imulu");
  setLibcallCallingConv(RTLIB::MUL_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I24_I8, "_imul_b");
  setLibcallCallingConv(RTLIB::MUL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I32, "_lmulu");
  setLibcallCallingConv(RTLIB::MUL_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I8, "_bdivs");
  setLibcallCallingConv(RTLIB::SDIV_I8, CallingConv::Z80_LibCall_BC);
  setLibcallName(RTLIB::SDIV_I16, "_sdivs");
  setLibcallCallingConv(RTLIB::SDIV_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I24, "_idivs");
  setLibcallCallingConv(RTLIB::SDIV_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I32, "_ldivs");
  setLibcallCallingConv(RTLIB::SDIV_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I8, "_bdivu");
  setLibcallCallingConv(RTLIB::UDIV_I8, CallingConv::Z80_LibCall_BC);
  setLibcallName(RTLIB::UDIV_I16, "_sdivu");
  setLibcallCallingConv(RTLIB::UDIV_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I24, "_idivu");
  setLibcallCallingConv(RTLIB::UDIV_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I32, "_ldivu");
  setLibcallCallingConv(RTLIB::UDIV_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I8, "_brems");
  setLibcallCallingConv(RTLIB::SREM_I8, CallingConv::Z80_LibCall_AC);
  setLibcallName(RTLIB::SREM_I16, "_srems");
  setLibcallCallingConv(RTLIB::SREM_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I24, "_irems");
  setLibcallCallingConv(RTLIB::SREM_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I32, "_lrems");
  setLibcallCallingConv(RTLIB::SREM_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UREM_I8, "_bremu");
  setLibcallCallingConv(RTLIB::UREM_I8, CallingConv::Z80_LibCall_AC);
  setLibcallName(RTLIB::UREM_I16, "_sremu");
  setLibcallCallingConv(RTLIB::UREM_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UREM_I24, "_iremu");
  setLibcallCallingConv(RTLIB::UREM_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UREM_I32, "_lremu");
  setLibcallCallingConv(RTLIB::UREM_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIVREM_I24, "_idvrmu");
  setLibcallCallingConv(RTLIB::UDIVREM_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIVREM_I32, "_ldvrmu");
  setLibcallCallingConv(RTLIB::UDIVREM_I32, CallingConv::Z80_LibCall);
}

// SelectionDAG Helpers

SDValue Z80TargetLowering::EmitOffset(int64_t Amount, const SDLoc &DL,
                                      SDValue Op, SelectionDAG &DAG) const {
  EVT VT = Op.getValueType();
  return DAG.getNode(ISD::ADD, DL, VT, Op, DAG.getConstant(Amount, DL, VT));
}

SDValue Z80TargetLowering::EmitNegate(const SDLoc &DL, SDValue Op,
                                      SelectionDAG &DAG) const {
  EVT VT = Op.getValueType();
  return DAG.getNode(ISD::SUB, DL, VT, DAG.getConstant(0, DL, VT), Op);
}

SDValue Z80TargetLowering::EmitFlipSign(const SDLoc &DL, SDValue Op,
                                        SelectionDAG &DAG) const {
  EVT VT = Op.getValueType();
  return DAG.getNode(ISD::ADD, DL, VT, Op, DAG.getConstant(
                         APInt::getSignBit(VT.getSizeInBits()), DL, VT));
}

// Legalize Types Helpers

void Z80TargetLowering::ReplaceNodeResults(SDNode *N,
                                           SmallVectorImpl<SDValue> &Results,
                                           SelectionDAG &DAG) const {
  DEBUG(dbgs() << "ReplaceNodeResults: "; N->dump(&DAG));
}

// Legalize Helpers

SDValue Z80TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  DEBUG(dbgs() << "LowerOperation: "; Op->dump(&DAG));
  assert(Op.getResNo() == 0);
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Don't know how to lower this operation.");
  case ISD::BR_CC:     return LowerBR_CC(Op, DAG);
  case ISD::SETCC:     return LowerSETCC(Op, DAG);
  case ISD::SELECT_CC: return LowerSELECT_CC(Op, DAG);
  case ISD::MUL:       return LowerMUL(Op, DAG);
  case ISD::SHL:       return LowerSHL(Op, DAG);
  case ISD::SRA:       return LowerSHR(true, Op, DAG);
  case ISD::SRL:       return LowerSHR(false, Op, DAG);
  case ISD::LOAD:      return LowerLoad(cast<LoadSDNode>(Op.getNode()), DAG);
  case ISD::STORE:     return LowerStore(cast<StoreSDNode>(Op.getNode()), DAG);
  }
}

SDValue Z80TargetLowering::LowerLoad(LoadSDNode *Node,
                                     SelectionDAG &DAG) const {
  SDLoc DL(Node);
  SDValue Ch = Node->getChain();
  SDValue Ptr = Node->getBasePtr();
  const MachinePointerInfo &MPI = Node->getPointerInfo();
  unsigned Alignment = Node->getAlignment();
  MachineMemOperand *MMO = Node->getMemOperand();
  AAMDNodes AAInfo = Node->getAAInfo();
  if (Node->getExtensionType() == ISD::EXTLOAD)
    return DAG.getLoad(Node->getValueType(0), DL, Ch, Ptr, MPI, Alignment,
                       MMO->getFlags(), AAInfo);
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  bool Split = Z80::splitReg(MMO->getSize(), MVT::i8, MVT::i16, MVT::i24,
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff,
                             Subtarget.hasEZ80Ops());
  assert(Split && "Can only custom lower splittable loads");
  SDValue Lo = DAG.getLoad(MVT::SimpleValueType(LoTy), DL, Ch, Ptr, MPI,
                           Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getMemBasePlusOffset(Ptr, HiOff, DL);
  SDValue Hi = DAG.getLoad(MVT::SimpleValueType(HiTy), DL, Ch, Ptr,
                           MPI.getWithOffset(HiOff), MinAlign(Alignment, HiOff),
                           MMO->getFlags(), AAInfo);
  Ch = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                   Lo.getValue(1), Hi.getValue(1));
  const SDValue Ops[] = { DAG.getTargetConstant(RC, DL, MVT::i32),
                          Lo, DAG.getTargetConstant(LoIdx, DL, MVT::i32),
                          Hi, DAG.getTargetConstant(HiIdx, DL, MVT::i32) };
  SDNode *Res = DAG.getMachineNode(TargetOpcode::REG_SEQUENCE, DL,
                                   Node->getValueType(0), Ops);
  return DAG.getMergeValues({ SDValue(Res, 0), Ch }, DL);
}

SDValue Z80TargetLowering::LowerStore(StoreSDNode *Node,
                                      SelectionDAG &DAG) const {
  SDLoc DL(Node);
  SDValue Ch = Node->getChain();
  SDValue Ptr = Node->getBasePtr();
  SDValue Val = Node->getValue();
  const MachinePointerInfo &MPI = Node->getPointerInfo();
  unsigned Alignment = Node->getAlignment();
  MachineMemOperand *MMO = Node->getMemOperand();
  AAMDNodes AAInfo = Node->getAAInfo();
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  bool Split = Z80::splitReg(MMO->getSize(), MVT::i8, MVT::i16, MVT::i24,
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff,
                             Subtarget.has16BitEZ80Ops());
  assert(Split && "Can only custom lower splittable stores");
  SDValue Lo = EmitExtractSubreg(LoIdx, DL, Val, DAG);
  Lo = DAG.getStore(Ch, DL, Lo, Ptr, MPI, Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getMemBasePlusOffset(Ptr, HiOff, DL);
  SDValue Hi = EmitExtractSubreg(HiIdx, DL, Val, DAG);
  Hi = DAG.getStore(Ch, DL, Hi, Ptr, MPI.getWithOffset(HiOff),
                    MinAlign(Alignment, HiOff), MMO->getFlags(), AAInfo);
  Ch = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                   Lo.getValue(0), Hi.getValue(0));
  return Ch;
}

SDValue Z80TargetLowering::EmitCmp(SDValue LHS, SDValue RHS, SDValue &TargetCC,
                                   ISD::CondCode CC, const SDLoc &DL,
                                   SelectionDAG &DAG) const {
  EVT VT = LHS.getValueType();
  assert(VT == RHS.getValueType() && "Types should match");
  assert(VT.isScalarInteger() && "Unhandled type");
  ConstantSDNode *Const = dyn_cast<ConstantSDNode>(RHS);
  int32_t SignVal = 1 << (VT.getSizeInBits() - 1), ConstVal;
  if (Const)
    ConstVal = Const->getSExtValue();
  Z80::CondCode TCC = Z80::COND_INVALID;
  unsigned Opc = Z80ISD::SUB;
  assert(!isa<ConstantSDNode>(LHS) && "Unexpected constant lhs");
  switch (CC) {
  default: llvm_unreachable("Invalid integer condition");
  case ISD::SETEQ:
    TCC = Z80::COND_Z;
    break;
  case ISD::SETNE:
    TCC = Z80::COND_NZ;
    break;
  case ISD::SETULE:
    if (Const) {
      assert(ConstVal != -1 && "Unexpected always true condition");
      ++ConstVal;
      TCC = Z80::COND_C;
      break;
    }
    Const = nullptr;
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETUGE:
    TCC = Z80::COND_NC;
    break;
  case ISD::SETUGT:
    if (Const) {
      assert(ConstVal != -1 && "Unexpected always false condition");
      ++ConstVal;
      TCC = Z80::COND_NC;
      break;
    }
    Const = nullptr;
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETULT:
    TCC = Z80::COND_C;
    break;
  case ISD::SETLE:
    Const = nullptr;
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETGE:
    LHS = EmitFlipSign(DL, LHS, DAG);
    if (Const)
      ConstVal ^= SignVal;
    else
      RHS = EmitFlipSign(DL, RHS, DAG);
    TCC = Z80::COND_NC;
    break;
  case ISD::SETGT:
    Const = nullptr;
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETLT:
    LHS = EmitFlipSign(DL, LHS, DAG);
    if (Const)
      ConstVal ^= SignVal;
    else
      RHS = EmitFlipSign(DL, RHS, DAG);
    TCC = Z80::COND_NC;
    break;
  }
  switch (TCC) {
  default: llvm_unreachable("Invalid target condition");
  case Z80::COND_Z:
  case Z80::COND_NZ:
    break;
  case Z80::COND_C:
  case Z80::COND_NC:
    // For word compares with constants, adding the negative is more optimal.
    if (VT != MVT::i8 && Const) {
      Opc = Z80ISD::ADD;
      TCC = Z80::GetOppositeBranchCondition(TCC);
      ConstVal = -ConstVal;
      if (ConstVal == SignVal) {
        RHS = LHS;
        Const = nullptr;
      }
    }
    break;
  }
  if (Const)
    RHS = DAG.getConstant(ConstVal, DL, VT);
  TargetCC = DAG.getConstant(TCC, DL, MVT::i8);
  return DAG.getNode(Opc, DL, DAG.getVTList(VT, MVT::i8), LHS, RHS).getValue(1);
}

// Old SelectionDAG helpers
SDValue Z80TargetLowering::EmitExtractSubreg(unsigned Idx, const SDLoc &DL,
                                             SDValue Op,
                                             SelectionDAG &DAG) const {
  //if (Op.getOpcode() != ISD::Constant)
  //  return DAG.getTargetExtractSubreg(Idx, DL, VT, Op);
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  return DAG.getNode(ISD::TRUNCATE, DL,
                     MVT::getIntegerVT(TRI->getSubRegIdxSize(Idx)),
                     DAG.getNode(ISD::SRL, DL, Op.getValueType(), Op,
                                 DAG.getConstant(TRI->getSubRegIdxOffset(Idx),
                                                 DL, MVT::i8)));
}
SDValue Z80TargetLowering::EmitInsertSubreg(unsigned Idx, const SDLoc &DL,
                                            MVT VT, SDValue Op,
                                            SelectionDAG &DAG) const {
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  assert(Op.getValueSizeInBits() == TRI->getSubRegIdxSize(Idx));
  return DAG.getNode(ISD::SHL, DL, VT,
                     DAG.getNode(ISD::ZERO_EXTEND, DL, VT, Op),
                     DAG.getConstant(TRI->getSubRegIdxOffset(Idx), DL,
                                     MVT::i8));
}

/// LowerAddSub - This handles splitting 32-bit operations unevenly on 24-bit.
SDValue Z80TargetLowering::LowerAddSubNew(SDValue Op, SelectionDAG &DAG) const {
  if (Op.getValueType() != MVT::i32 || !Subtarget.is24Bit())
    return Op;
  unsigned OpcC, OpcE;
  MVT GlueVT;
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Unexpected opcode");
  case ISD::ADD:
  case ISD::ADDC:
    OpcC = ISD::ADDC;
    OpcE = ISD::ADDE;
    GlueVT = MVT::Glue;
    break;
  case ISD::ADDE:
    OpcC = OpcE = ISD::ADDE;
    GlueVT = MVT::Glue;
    break;
  case ISD::SUB:
  case ISD::SUBC:
    OpcC = ISD::SUBC;
    OpcE = ISD::SUBE;
    GlueVT = MVT::Glue;
    break;
  case ISD::SUBE:
    OpcC = OpcE = ISD::SUBE;
    GlueVT = MVT::Glue;
    break;
  case Z80ISD::ADD:
    OpcC = Z80ISD::ADD;
    OpcE = Z80ISD::ADC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::ADC:
    OpcC = OpcE = Z80ISD::ADC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::SUB:
    OpcC = Z80ISD::SUB;
    OpcE = Z80ISD::SBC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::SBC:
    OpcC = OpcE = Z80ISD::SBC;
    GlueVT = MVT::i8;
    break;
  }
  SDLoc DL(Op);
  SDValue Ops[3];
  unsigned OpCount = 2;
  for (OpCount = 0; OpCount != 2; ++OpCount)
    Ops[OpCount] = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i24,
                               Op.getOperand(OpCount),
                               DAG.getIntPtrConstant(0, DL));
  if (OpcC == OpcE)
    Ops[OpCount++] = Op.getOperand(2);
  SDValue ResLo = DAG.getNode(OpcC, DL, DAG.getVTList(MVT::i24, GlueVT),
                              makeArrayRef(Ops, OpCount));
  for (unsigned I = 0; I != 2; ++I)
    Ops[I] = DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i8, Op.getOperand(I),
                         DAG.getIntPtrConstant(1, DL));
  Ops[2] = ResLo.getValue(1);
  SDValue ResHi = DAG.getNode(OpcE, DL, DAG.getVTList(MVT::i8, GlueVT), Ops);
  Ops[0] = DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i32, ResLo, ResHi);
  Ops[1] = ResHi.getValue(1);
  return DAG.getMergeValues(makeArrayRef(Ops, 2), DL);
}
SDValue Z80TargetLowering::LowerAddSub(SDValue Op, SelectionDAG &DAG) const {
  if (Op.getValueType() != MVT::i32 || !Subtarget.is24Bit())
    return Op;
  unsigned OpcC, OpcE;
  MVT GlueVT;
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Unexpected opcode");
  case ISD::ADD:
  case ISD::ADDC:
    OpcC = ISD::ADDC;
    OpcE = ISD::ADDE;
    GlueVT = MVT::Glue;
    break;
  case ISD::ADDE:
    OpcC = OpcE = ISD::ADDE;
    GlueVT = MVT::Glue;
    break;
  case ISD::SUB:
  case ISD::SUBC:
    OpcC = ISD::SUBC;
    OpcE = ISD::SUBE;
    GlueVT = MVT::Glue;
    break;
  case ISD::SUBE:
    OpcC = OpcE = ISD::SUBE;
    GlueVT = MVT::Glue;
    break;
  case Z80ISD::ADD:
    OpcC = Z80ISD::ADD;
    OpcE = Z80ISD::ADC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::ADC:
    OpcC = OpcE = Z80ISD::ADC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::SUB:
    OpcC = Z80ISD::SUB;
    OpcE = Z80ISD::SBC;
    GlueVT = MVT::i8;
    break;
  case Z80ISD::SBC:
    OpcC = OpcE = Z80ISD::SBC;
    GlueVT = MVT::i8;
    break;
  }
  SDLoc DL(Op);
  SDValue Ops[3];
  unsigned OpCount;
  SDValue LHS = Op.getOperand(0), RHS = Op.getOperand(1);
  SDValue LHSLo = EmitExtractSubreg(Z80::sub_long, DL, LHS, DAG);
  SDValue RHSLo = EmitExtractSubreg(Z80::sub_long, DL, RHS, DAG);
  SDValue LHSHi = EmitExtractSubreg(Z80::sub_top,  DL, LHS, DAG);
  SDValue RHSHi = EmitExtractSubreg(Z80::sub_top,  DL, RHS, DAG);
  Ops[0] = LHSLo;
  Ops[1] = RHSLo;
  OpCount = 2;
  if (OpcC == OpcE) {
    Ops[2] = Op.getOperand(2);
    OpCount = 3;
  }
  SDValue ResLo = DAG.getNode(OpcC, DL, DAG.getVTList(MVT::i24, GlueVT),
                              makeArrayRef(Ops, OpCount));
  SDValue ResHi = DAG.getNode(OpcE, DL, DAG.getVTList(MVT::i8, GlueVT),
                              LHSHi, RHSHi, ResLo.getValue(1));
  Ops[1] = ResHi.getValue(1);
  ResLo = EmitInsertSubreg(Z80::sub_long, DL, MVT::i32, ResLo, DAG);
  ResHi = EmitInsertSubreg(Z80::sub_top,  DL, MVT::i32, ResHi, DAG);
  Ops[0] = DAG.getNode(ISD::OR, DL, MVT::i32, ResLo, ResHi);
  return DAG.getMergeValues(makeArrayRef(Ops, 2), DL);
}

SDValue Z80TargetLowering::LowerADDSUB(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);
  /*if (Op.getValueType() != MVT::i32) {
    assert(Op.getValueType() == MVT::i16 ||
           Op.getValueType() == MVT::i24);
    switch (Op.getOpcode()) {
    default: llvm_unreachable("Can only handle adds/subs");
    case ISD::ADD:
    case ISD::SUB:
      asm("int3");
      unsigned Opc = Op.getOpcode() == ISD::ADD ? ISD::ADDE : ISD::SUBE;
      return DAG.getNode(Opc, DL, DAG.getVTList(Op.getValueType(), MVT::Glue),
                         L, R, DAG.getNode(ISD::CARRY_FALSE, DL, MVT::Glue));
    }
  }*/
  assert(Op.getValueType() == MVT::i32 && "Unexpected type");
  unsigned OpC, OpE;
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Can only handle adds/subs");
  case ISD::ADD: case ISD::ADDC: OpC = ISD::ADDC; OpE = ISD::ADDE; break;
  case ISD::ADDE: OpC = OpE = ISD::ADDE; break;
  case ISD::SUB: case ISD::SUBC: OpC = ISD::SUBC; OpE = ISD::SUBE; break;
  case ISD::SUBE: OpC = OpE = ISD::ADDE; break;
  case Z80ISD::ADD: OpC = Z80ISD::ADD; OpE = Z80ISD::ADC; break;
  case Z80ISD::ADC: OpC = OpE = Z80ISD::ADC; break;
  case Z80ISD::SUB: OpC = Z80ISD::SUB; OpE = Z80ISD::SBC; break;
  case Z80ISD::SBC: OpC = OpE = Z80ISD::SBC; break;
  }
  SDValue LH = EmitExtractSubreg(Z80::sub_top,  DL, L, DAG);
  SDValue LL = EmitExtractSubreg(Z80::sub_long, DL, L, DAG);
  SDValue RH = EmitExtractSubreg(Z80::sub_top,  DL, R, DAG);
  SDValue RL = EmitExtractSubreg(Z80::sub_long, DL, R, DAG);
  SDValue Ops[3] = { LL, RL };
  unsigned OpCount = 2;
  if (OpC == OpE)
    Ops[OpCount++] = Op.getOperand(2);
  SDValue Lo = DAG.getNode(OpC, DL, DAG.getVTList(MVT::i24, MVT::Glue),
                           makeArrayRef(Ops, OpCount));
  SDValue Hi = DAG.getNode(OpE, DL, DAG.getVTList(MVT::i8,  MVT::Glue),
                           LH, RH, Lo.getValue(1));
  SDValue Result = DAG.getUNDEF(MVT::i16);
  Result = DAG.getTargetInsertSubreg(Z80::sub_long, DL, MVT::i32, Result, Lo);
  Result = DAG.getTargetInsertSubreg(Z80::sub_top,  DL, MVT::i32, Result, Hi);
  Ops[0] = Result;
  Ops[1] = Hi.getValue(1);
  return DAG.getMergeValues(makeArrayRef(Ops, 2), DL);
}

SDValue Z80TargetLowering::LowerSHL(SDValue Op, SelectionDAG &DAG) const {
  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  if (ConstantSDNode *ShiftAmountNode =
      dyn_cast<ConstantSDNode>(Op.getOperand(1))) {
    uint64_t ShiftAmount = ShiftAmountNode->getZExtValue();
    SDValue Res = Op.getOperand(0);
    while (ShiftAmount--)
      if (VT == MVT::i8)
        Res = DAG.getNode(Z80ISD::SLA, DL, DAG.getVTList(MVT::i8, MVT::i8), Res);
      else
        Res = DAG.getNode(ISD::ADD, DL, VT, Res, Res);
    return Res;
  }
  return DAG.getNode(ISD::SHL, DL, VT, Op.getOperand(0), Op.getOperand(1));
}
SDValue Z80TargetLowering::LowerSHR(bool Signed, SDValue Op,
                                    SelectionDAG &DAG) const {
  EVT VT = Op.getValueType();
  assert(VT.getSizeInBits() <= 32 && VT.getSizeInBits() % 8 == 0 &&
         "Can only handle multiple of byte sized operations");
  SDLoc DL(Op);
  SmallVector<SDValue, 4> Bytes;
  for (unsigned Shift = VT.getSizeInBits(); Shift; Shift -= 8)
    Bytes.push_back(DAG.getNode(ISD::TRUNCATE, DL, MVT::i8,
                                DAG.getNode(ISD::SRL, DL, VT, Op.getOperand(0),
                                            DAG.getConstant(Shift-8, DL, VT))));
  if (ConstantSDNode *ShiftAmountNode =
      dyn_cast<ConstantSDNode>(Op.getOperand(1))) {
    uint64_t ShiftAmount = ShiftAmountNode->getZExtValue();
    while (ShiftAmount--) {
      Bytes[0] = DAG.getNode(Signed ? Z80ISD::SRA : Z80ISD::SRL, DL,
                             DAG.getVTList(MVT::i8, MVT::i8), Bytes[0]);
      for (unsigned I = 1, E = Bytes.size(); I != E; ++I)
        Bytes[I] = DAG.getNode(Z80ISD::RR, DL, DAG.getVTList(MVT::i8, MVT::i8),
                               Bytes[I], Bytes[I-1].getValue(1));
    }
    SDValue Res = DAG.getConstant(0, DL, VT);
    for (unsigned Shift = VT.getSizeInBits(); Shift; Shift -= 8)
      Res = DAG.getNode(ISD::OR, DL, VT, Res,
                        DAG.getNode(ISD::SHL, DL, VT,
                                    DAG.getNode(ISD::ZERO_EXTEND, DL, VT,
                                                Bytes.pop_back_val()),
                                    DAG.getConstant(Shift-8, DL, VT)));
    return Res;
  }
  return DAG.getNode(Signed ? ISD::SRA : ISD::SRL, DL, VT,
                     Op.getOperand(0), Op.getOperand(1));
}

SDValue Z80TargetLowering::LowerMUL(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Result = DAG.getUNDEF(MVT::i16);
  Result = DAG.getTargetInsertSubreg(Z80::sub_low,  DL, MVT::i16, Result,
                                     Op.getOperand(0));
  Result = DAG.getTargetInsertSubreg(Z80::sub_high, DL, MVT::i16, Result,
                                     Op.getOperand(1));
  Result = DAG.getNode(Z80ISD::MLT, DL, MVT::i16, Result);
  return EmitExtractSubreg(Z80::sub_low, DL, Result, DAG);
}

SDValue Z80TargetLowering::EmitCMP(SDValue LHS, SDValue RHS, SDValue &TargetCC,
                ISD::CondCode CC, const SDLoc &DL, SelectionDAG &DAG) const {
  EVT VT = LHS.getValueType();
  assert(RHS.getValueType() == VT);
  assert(!VT.isFloatingPoint() && "We don't handle FP yet");

  Z80::CondCode TCC = Z80::COND_INVALID;
  unsigned Opc = Z80ISD::SUB;
  switch (CC) {
  default: llvm_unreachable("Invalid integer condition!");
  case ISD::SETEQ:
    TCC = Z80::COND_Z;
    // Minor optimization: if LHS is a constant, swap operands, then the
    // constant can be folded into comparison.
    if (LHS.getOpcode() == ISD::Constant)
      std::swap(LHS, RHS);
    break;
  case ISD::SETNE:
    TCC = Z80::COND_NZ;
    // Minor optimization: if LHS is a constant, swap operands, then the
    // constant can be folded into comparison.
    if (LHS.getOpcode() == ISD::Constant)
      std::swap(LHS, RHS);
    break;
  case ISD::SETGE:
    std::swap(LHS, RHS);
  case ISD::SETLE:
    LHS = EmitFlipSign(DL, LHS, DAG);
    RHS = EmitFlipSign(DL, RHS, DAG);
    LLVM_FALLTHROUGH;
  case ISD::SETULE:
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETUGE:
    // Turn lhs u>= rhs with lhs constant into rhs u< lhs+1, this allows us to
    // fold constant into instruction.
    if (LHS.getOpcode() == ISD::Constant) {
      std::swap(LHS, RHS);
      RHS = EmitOffset(1, DL, RHS, DAG);
      TCC = Z80::COND_C;
      break;
    }
    TCC = Z80::COND_NC;
    break;
  case ISD::SETLT:
    std::swap(LHS, RHS);
  case ISD::SETGT:
    LHS = EmitFlipSign(DL, LHS, DAG);
    RHS = EmitFlipSign(DL, RHS, DAG);
    LLVM_FALLTHROUGH;
  case ISD::SETUGT:
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETULT:
    // Turn lhs u< rhs with lhs constant into rhs u>= lhs+1, this allows us to
    // fold constant into instruction.
    if (LHS.getOpcode() == ISD::Constant) {
      std::swap(LHS, RHS);
      RHS = EmitOffset(1, DL, RHS, DAG);
      TCC = Z80::COND_NC;
      break;
    }
    TCC = Z80::COND_C;
    break;
  }

  // For word compares, adding the negative is more optimal for constants.
  if ((TCC == Z80::COND_C || TCC == Z80::COND_NC) && VT != MVT::i8 &&
      RHS.getOpcode() == ISD::Constant) {
    Opc = Z80ISD::ADD;
    RHS = EmitNegate(DL, RHS, DAG);
    TCC = Z80::GetOppositeBranchCondition(TCC);
  }
  TargetCC = DAG.getConstant(TCC, DL, MVT::i8);
  SDVTList VTs = DAG.getVTList(VT, MVT::i8);
  SDValue Cmp = DAG.getNode(Opc, DL, VTs, LHS, RHS);
  if (VT == MVT::i32)
    Cmp = LowerADDSUB(Cmp, DAG);
  return Cmp.getValue(1);
}

SDValue Z80TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS   = Op.getOperand(2);
  SDValue RHS   = Op.getOperand(3);
  SDValue Dest  = Op.getOperand(4);
  SDLoc DL(Op);

  SDValue TargetCC;
  SDValue Flags = EmitCmp(LHS, RHS, TargetCC, CC, DL, DAG);

  return DAG.getNode(Z80ISD::BRCOND, DL, Op.getValueType(),
                     Chain, Dest, TargetCC, Flags);
}

SDValue Z80TargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDLoc DL(Op);

  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  SDValue TargetCC;
  SDValue Flags = EmitCmp(LHS, RHS, TargetCC, CC, DL, DAG);

  EVT VT = Op.getValueType();
  return DAG.getNode(Z80ISD::SELECT, DL, DAG.getVTList(VT, MVT::Glue),
                     DAG.getConstant(1, DL, VT), DAG.getConstant(0, DL, VT),
                     TargetCC, Flags);
}

SDValue Z80TargetLowering::LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TV  = Op.getOperand(2);
  SDValue FV  = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDLoc DL(Op);

  SDValue TargetCC;
  SDValue Flag = EmitCmp(LHS, RHS, TargetCC, CC, DL, DAG);

  return DAG.getNode(Z80ISD::SELECT, DL,
                     DAG.getVTList(Op.getValueType(), MVT::Glue), TV, FV,
                     TargetCC, Flag);
}

SDValue Z80TargetLowering::LowerLOAD(LoadSDNode *Node,
                                     SelectionDAG &DAG) const {
  SDLoc DL(Node);
  SDValue Ch = Node->getChain();
  SDValue Ptr = Node->getBasePtr();
  const MachinePointerInfo &MPI = Node->getPointerInfo();
  unsigned Alignment = Node->getAlignment();
  MachineMemOperand *MMO = Node->getMemOperand();
  AAMDNodes AAInfo = Node->getAAInfo();
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  bool Split = Z80::splitReg(MMO->getSize(), MVT::i8, MVT::i16, MVT::i24,
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff,
                             Subtarget.hasEZ80Ops());
  assert(Split && "Can only custom lower splittable loads");
  SDValue Lo = DAG.getLoad(MVT::SimpleValueType(LoTy), DL, Ch, Ptr, MPI,
                           Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getMemBasePlusOffset(Ptr, HiOff, DL);
  SDValue Hi = DAG.getLoad(MVT::SimpleValueType(HiTy), DL, Ch, Ptr,
                           MPI.getWithOffset(HiOff), MinAlign(Alignment, HiOff),
                           MMO->getFlags(), AAInfo);
  Ch = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                   Lo.getValue(1), Hi.getValue(1));
  const SDValue Ops[] = { DAG.getTargetConstant(RC, DL, MVT::i32),
                          Lo, DAG.getTargetConstant(LoIdx, DL, MVT::i32),
                          Hi, DAG.getTargetConstant(HiIdx, DL, MVT::i32) };
  SDNode *Res = DAG.getMachineNode(TargetOpcode::REG_SEQUENCE, DL,
                                   Node->getValueType(0), Ops);
  return DAG.getMergeValues({ SDValue(Res, 0), Ch }, DL);
}
SDValue Z80TargetLowering::LowerSTORE(StoreSDNode *Node,
                                      SelectionDAG &DAG) const {
  SDLoc DL(Node);
  SDValue Ch = Node->getChain();
  SDValue Ptr = Node->getBasePtr();
  SDValue Val = Node->getValue();
  const MachinePointerInfo &MPI = Node->getPointerInfo();
  unsigned Alignment = Node->getAlignment();
  MachineMemOperand *MMO = Node->getMemOperand();
  AAMDNodes AAInfo = Node->getAAInfo();
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  bool Split = Z80::splitReg(MMO->getSize(), MVT::i8, MVT::i16, MVT::i24,
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff,
                             Subtarget.has16BitEZ80Ops());
  assert(Split && "Can only custom lower splittable stores");
  SDValue Lo = EmitExtractSubreg(LoIdx, DL, Val, DAG);
  Lo = DAG.getStore(Ch, DL, Lo, Ptr, MPI, Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getMemBasePlusOffset(Ptr, HiOff, DL);
  SDValue Hi = EmitExtractSubreg(HiIdx, DL, Val, DAG);
  Hi = DAG.getStore(Ch, DL, Hi, Ptr, MPI.getWithOffset(HiOff),
                    MinAlign(Alignment, HiOff), MMO->getFlags(), AAInfo);
  Ch = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                   Lo.getValue(0), Hi.getValue(0));
  return Ch;
}

SDValue Z80TargetLowering::LowerOperationOld(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::ADD: case ISD::SUB:
  case ISD::ADDC: case ISD::SUBC:
  case ISD::ADDE: case ISD::SUBE:
  case Z80ISD::ADD: case Z80ISD::SUB:
  case Z80ISD::ADC: case Z80ISD::SBC: return LowerAddSubNew(Op, DAG);
  case ISD::SHL: return LowerLibCall(RTLIB::SHL_I8, RTLIB::SHL_I16, RTLIB::SHL_I24, RTLIB::SHL_I32, Op, DAG);//return LowerSHL(Op, DAG);
  case ISD::SRA: return LowerLibCall(RTLIB::SRA_I8, RTLIB::SRA_I16, RTLIB::SRA_I24, RTLIB::SRA_I32, Op, DAG);//return LowerSHR(true, Op, DAG);
  case ISD::SRL: return LowerLibCall(RTLIB::SRL_I8, RTLIB::SRL_I16, RTLIB::SRL_I24, RTLIB::SRL_I32, Op, DAG);//return LowerSHR(false, Op, DAG);
  case ISD::MUL: return LowerMUL(Op, DAG);
  case ISD::BR_CC: return LowerBR_CC(Op, DAG);
  case ISD::SETCC: return LowerSETCC(Op, DAG);
  case ISD::SELECT_CC: return LowerSELECT_CC(Op, DAG);
  case ISD::LOAD: return LowerLOAD(cast<LoadSDNode>(Op), DAG);
  case ISD::STORE: return LowerSTORE(cast<StoreSDNode>(Op), DAG);
  }
  if (Op.getValueType() == MVT::i16) {
    switch (Op.getOpcode()) {
    default: llvm_unreachable("Should not custom lower this!");
    case ISD::AND:
    case ISD::XOR:
    case ISD:: OR: return NarrowOperation(Op, DAG);
    }
  } else {
    switch (Op.getOpcode()) {
    default: llvm_unreachable("Should not custom lower this!");
    case ISD::AND: return LowerLibCall(RTLIB::UNKNOWN_LIBCALL, RTLIB::AND_I16,
                                       RTLIB::AND_I24, RTLIB::AND_I32, Op, DAG);
    case ISD::XOR: return LowerLibCall(RTLIB::UNKNOWN_LIBCALL, RTLIB::XOR_I16,
                                       RTLIB::XOR_I24, RTLIB::XOR_I32, Op, DAG);
    case ISD:: OR: return LowerLibCall(RTLIB::UNKNOWN_LIBCALL, RTLIB:: OR_I16,
                                       RTLIB:: OR_I24, RTLIB:: OR_I32, Op, DAG);
    }
  }
}

SDValue Z80TargetLowering::NarrowOperation(SDValue Op, SelectionDAG &DAG) const {
  assert(Op.getValueType() == MVT::i16 && "Can only narrow i16 operations");
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);
  SDValue LH = EmitExtractSubreg(Z80::sub_high, DL, L, DAG);
  SDValue LL = EmitExtractSubreg(Z80::sub_low,  DL, L, DAG);
  SDValue RH = EmitExtractSubreg(Z80::sub_high, DL, R, DAG);
  SDValue RL = EmitExtractSubreg(Z80::sub_low,  DL, R, DAG);
  SDValue Lo = DAG.getNode(Op.getOpcode(), DL, MVT::i8, LL, RL);
  SDValue Result = DAG.getUNDEF(MVT::i16);
  SDValue Hi = DAG.getNode(Op.getOpcode(), DL, MVT::i8, LH, RH);
  Result = DAG.getTargetInsertSubreg(Z80::sub_low,  DL, MVT::i16, Result, Lo);
  Result = DAG.getTargetInsertSubreg(Z80::sub_high, DL, MVT::i16, Result, Hi);
  return Result;
}

SDValue Z80TargetLowering::LowerLibCall(
    RTLIB::Libcall LC8, RTLIB::Libcall LC16, RTLIB::Libcall LC24,
    RTLIB::Libcall LC32, SDValue Op, SelectionDAG &DAG) const {
  RTLIB::Libcall LC;
  assert(Op.getSimpleValueType().isInteger() && "Can not lower this type");
  switch (Op.getValueSizeInBits()) {
  default: llvm_unreachable("Can not lower this type");
  case 8:  LC = LC8;  break;
  case 16: LC = LC16; break;
  case 24: LC = LC24; break;
  case 32: LC = LC32; break;
  }

  SmallVector<SDValue, 2> Ops;
  Ops.reserve(Op.getNumOperands());
  for (unsigned I = 0, E = Op.getNumOperands(); I != E; ++I)
    Ops.push_back(Op.getOperand(I));
  return makeLibCall(DAG, LC, Op.getValueType(), Ops, false, SDLoc(Op)).first;
}

/// Return true if the addressing mode represented by AM is legal for this
/// target, for a load/store of the specified type.
bool Z80TargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                              const AddrMode &AM, Type *Ty,
                                              unsigned AS) const {
  if (AM.Scale)
    return false;

  if (AM.BaseGV && AM.HasBaseReg)
    return false;

  if (AM.BaseGV)
    return AM.BaseOffs == 0;

  if (AM.HasBaseReg)
    return isInt<8>(AM.BaseOffs);

  return false;
}

bool Z80TargetLowering::isLegalICmpImmediate(int64_t Imm) const {
  return isInt<8>(Imm);
}
bool Z80TargetLowering::isLegalAddImmediate(int64_t Imm) const {
  return isInt<8>(Imm);
}

bool Z80TargetLowering::isTruncateFree(Type *Ty1, Type *Ty2) const {
  if (!Ty1->isIntegerTy() || !Ty2->isIntegerTy())
    return false;
  return Ty1->getPrimitiveSizeInBits() > Ty2->getPrimitiveSizeInBits();
}
bool Z80TargetLowering::isTruncateFree(EVT VT1, EVT VT2) const {
  if (!VT1.isInteger() || !VT2.isInteger())
    return false;
  return VT1.getSizeInBits() > VT2.getSizeInBits();
}

bool Z80TargetLowering::isZExtFree(Type *Ty1, Type *Ty2) const {
  // ez80 implicitly zero-extends 16-bit results in 24-bit registers.
  return Ty1->isIntegerTy(16) && Ty2->isIntegerTy(24) && Subtarget.is24Bit();
}
bool Z80TargetLowering::isZExtFree(EVT VT1, EVT VT2) const {
  // ez80 implicitly zero-extends 16-bit results in 24-bit registers.
  return VT1 == MVT::i16 && VT2 == MVT::i24 && Subtarget.is24Bit();
}

bool Z80TargetLowering::isNarrowingProfitable(EVT VT1, EVT VT2) const {
  // i16 instructions are longer (prefixed) and slower.
  return !(VT1 == MVT::i24 && VT2 == MVT::i16);
}

/// \brief Returns true if it is beneficial to convert a load of a constant
/// to just the constant itself.
bool Z80TargetLowering::shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                                          Type *Ty) const {
  assert(Ty->isIntegerTy());
  // Immediate loads are always the same size (or smaller) and faster than
  // a constant load (from an immediate address).
  return true;
}

/// Replace a node with an illegal result type with a new node built out of
/// custom code.
void Z80TargetLowering::ReplaceNodeResultsOld(SDNode *N,
                                           SmallVectorImpl<SDValue> &Results,
                                           SelectionDAG &DAG) const {
  return;
  SDLoc DL(N);
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  if (Z80::splitReg(N->getValueType(0).getStoreSize(), MVT::i8, MVT::i16, MVT::i24,
                    RC, LoTy, LoIdx, HiTy, HiIdx, HiOff,
                    Subtarget.has16BitEZ80Ops())) {

  }
  switch (N->getValueSizeInBits(0)) {
  case 32:
    Results.push_back(DAG.getNode(ISD::EXTRACT_ELEMENT, DL, MVT::i24, SDValue(N, 0)));
  }
  return;
  switch (N->getOpcode()) {
  default:
    llvm_unreachable("Don't know how to custom type legalize this operation!");
  case ISD::ADD: case ISD::SUB:
  case ISD::ADDC: case ISD::SUBC:
  case ISD::ADDE: case ISD::SUBE:
  case Z80ISD::ADD: case Z80ISD::SUB:
  case Z80ISD::ADC: case Z80ISD::SBC:
    SDValue Res = LowerAddSubNew(SDValue(N, 0), DAG);
    Results.push_back(Res);
    Results.push_back(Res.getValue(1));
    break;
  //case ISD::AND:
  //case ISD::XOR:
  //case ISD:: OR: return;
  }
}

SDValue Z80TargetLowering::combineCopyFromReg(SDNode *N,
                                              DAGCombinerInfo &DCI) const {
  SDLoc DL(N);
  SelectionDAG &DAG = DCI.DAG;
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  RegisterSDNode *Reg = cast<RegisterSDNode>(N->getOperand(1).getNode());
  if (Reg->getReg() == getStackPointerRegisterToSaveRestore() &&
      !MFI.hasVarSizedObjects()) {
    SDValue Ops[] = { DAG.getTargetIndex(Reg->getReg(), N->getValueType(0)),
                      N->getOperand(0) };
    return DAG.getMergeValues(Ops, DL);
  }
  return SDValue();
}

SDValue Z80TargetLowering::combineStore(StoreSDNode *N,
                                        DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineMemOperand *MMO = N->getMemOperand();
  const PseudoSourceValue *PSV = MMO->getPseudoValue();
  if (PSV && PSV->isStack()) {
    dbgs() << DCI.getDAGCombineLevel() << ':' << MFI.hasVarSizedObjects() << '\n';
  }
  return SDValue();
}

SDValue Z80TargetLowering::combineEXTRACT_SUBREG(SDNode *N,
                                                 DAGCombinerInfo &DCI) const {
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  SelectionDAG &DAG = DCI.DAG;
  EVT VT = N->getValueType(0);
  SDValue N0 = N->getOperand(0);
  unsigned Off = TRI->getSubRegIdxOffset(N->getConstantOperandVal(1));
  if (N0->getOpcode() == ISD::Constant)
    return DAG.getNode(ISD::TRUNCATE, SDLoc(N), VT,
                       DAG.getNode(ISD::SRL, SDLoc(N), VT, N0,
                                   DAG.getConstant(Off, SDLoc(N), VT)));
  return SDValue();
}

SDValue Z80TargetLowering::combineINSERT_SUBREG(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  return SDValue();
}

// This code is from DAGCombiner::visitADDC
SDValue Z80TargetLowering::combineADD(SDNode *N, DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  EVT VT = N->getValueType(0);
  SDValue N0 = N->getOperand(0);
  SDValue N1 = N->getOperand(1);

  // If the flag result is dead, turn this into an ADD.
  if (!N->hasAnyUseOfValue(1))
    return DCI.CombineTo(N, DAG.getNode(ISD::ADD, SDLoc(N), VT, N0, N1),
                         DAG.getNode(ISD::CARRY_FALSE, SDLoc(N), MVT::Glue));

  // canonicalize constant to RHS.
  ConstantSDNode *N0C = dyn_cast<ConstantSDNode>(N0);
  ConstantSDNode *N1C = dyn_cast<ConstantSDNode>(N1);
  if (N0C && !N1C)
    return DAG.getNode(N->getOpcode(), SDLoc(N), N->getVTList(), N1, N0);

  // fold (addc x, 0) -> x + no carry out
  if (isNullConstant(N1))
    return DCI.CombineTo(N, N0, DAG.getNode(ISD::CARRY_FALSE,
                                            SDLoc(N), MVT::Glue));
  return SDValue();
}

// This code is from DAGCombiner::visitSUBC
SDValue Z80TargetLowering::combineSUB(SDNode *N, DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  EVT VT = N->getValueType(0);
  SDValue N0 = N->getOperand(0);
  SDValue N1 = N->getOperand(1);

  // If the flag result is dead, turn this into an ADD.
  if (!N->hasAnyUseOfValue(1))
    return DCI.CombineTo(N, DAG.getNode(ISD::SUB, SDLoc(N), VT, N0, N1),
                         DAG.getNode(ISD::CARRY_FALSE, SDLoc(N), MVT::Glue));

  // fold (subc x, x) -> 0 + no borrow
  if (N0 == N1)
    return DCI.CombineTo(N, DAG.getConstant(0, SDLoc(N), VT),
                         DAG.getNode(ISD::CARRY_FALSE, SDLoc(N), MVT::Glue));

  // fold (subc x, 0) -> x + no borrow
  if (isNullConstant(N1))
    return DCI.CombineTo(N, N0, DAG.getNode(ISD::CARRY_FALSE, SDLoc(N),
                                            MVT::Glue));

  return SDValue();
}

SDValue Z80TargetLowering::PerformDAGCombine(SDNode *N,
                                             DAGCombinerInfo &DCI) const {
  return SDValue();
  switch (N->getOpcode()) {
  default:               return SDValue();
//case ISD::CopyFromReg: return combineCopyFromReg(N, DCI);
//case ISD::STORE:       return combineStore(cast<StoreSDNode>(N), DCI);
//case TargetOpcode::EXTRACT_SUBREG: return combineEXTRACT_SUBREG(N, DCI);
//case TargetOpcode::INSERT_SUBREG:  return combineINSERT_SUBREG(N, DCI);
//case Z80ISD::ADD:                  return combineADD(N, DCI);
//case Z80ISD::SUB:                  return combineSUB(N, DCI);
  }
}

/// Return true if the target has native support for the specified value type
/// and it is 'desirable' to use the type for the given node type. e.g. On x86
/// i16 is legal, but undesirable since i16 instruction encodings are longer and
/// some i16 instructions are slow.
bool Z80TargetLowering::isTypeDesirableForOp(unsigned Opc, EVT VT) const {
  if (!isTypeLegal(VT))
    return false;
  if (VT != MVT::i16 || Subtarget.is16Bit())
    return true;

  switch (Opc) {
  default:
    return true;
  case ISD::LOAD:
  case ISD::STORE:
  case ISD::SIGN_EXTEND:
  case ISD::ZERO_EXTEND:
  case ISD::ANY_EXTEND:
  case ISD::SHL:
  case ISD::SRL:
  case ISD::SUB:
  case ISD::ADD:
  case ISD::MUL:
  case ISD::AND:
  case ISD::OR:
  case ISD::XOR:
    return false;
  }
}

/// Return true if x op y -> (SrcVT)((DstVT)x op (DstVT)y) is beneficial.
bool Z80TargetLowering::isDesirableToShrinkOp(unsigned Opc, EVT SrcVT,
                                              EVT DstVT) const {
  switch (Opc) {
  default:
    return false;
  case ISD::ADD:
  case ISD::SUB:
  case ISD::ADDC:
  case ISD::SUBC:
  case ISD::ADDE:
  case ISD::SUBE:
    // These require a .sis suffix for i24 -> i16
    return DstVT != MVT::i16 || Subtarget.is16Bit();
  case ISD::AND:
  case ISD::OR:
  case ISD::XOR:
  case ISD::MUL:
    // These are more expensive on larger types, so always shrink.
    return true;
  }
}

/// This method query the target whether it is beneficial for dag combiner to
/// promote the specified node. If true, it should return the desired promotion
/// type by reference.
bool Z80TargetLowering::IsDesirableToPromoteOp(SDValue Op, EVT &PVT) const {
  EVT VT = Op.getValueType();
  if (VT != MVT::i16 || Subtarget.is16Bit())
    return false;
  PVT = MVT::i24;
  return true;
}

/// Return true if the MachineFunction contains a COPY which would imply
/// HasOpaqueSPAdjustment.
bool Z80TargetLowering::hasCopyImplyingStackAdjustment(
    MachineFunction *MF) const {
  return true; // TODO: implement
}

MachineBasicBlock *
Z80TargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                               MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unexpected instr type to insert");
  case Z80::Sub016:
  case Z80::Sub024:
    return EmitLoweredSub0(MI, BB);
  case Z80::Sub16:
  case Z80::Sub24:
    return EmitLoweredSub(MI, BB);
  case Z80::Cp016:
  case Z80::Cp024:
    return EmitLoweredCmp0(MI, BB);
  case Z80::Cp16:
  case Z80::Cp24:
    return EmitLoweredCmp(MI, BB);
  case Z80::Select8:
  case Z80::Select16:
  case Z80::Select24:
    return EmitLoweredSelect(MI, BB);
  }
}

void Z80TargetLowering::AdjustInstrPostInstrSelection(MachineInstr &MI,
                                                      SDNode *Node) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unexpected instr type to insert");
  case Z80::ADJCALLSTACKUP16:
  case Z80::ADJCALLSTACKUP24:
  case Z80::ADJCALLSTACKDOWN16:
  case Z80::ADJCALLSTACKDOWN24:
    return AdjustAdjCallStack(MI);
  }
}

void Z80TargetLowering::AdjustAdjCallStack(MachineInstr &MI) const {
  bool Is24Bit = MI.getOpcode() == Z80::ADJCALLSTACKUP24 ||
                 MI.getOpcode() == Z80::ADJCALLSTACKDOWN24;
  assert((Is24Bit || MI.getOpcode() == Z80::ADJCALLSTACKUP16 ||
                     MI.getOpcode() == Z80::ADJCALLSTACKDOWN16) &&
         "Unexpected opcode");
  MachineRegisterInfo &MRI = MI.getParent()->getParent()->getRegInfo();
  unsigned Reg = MRI.createVirtualRegister(Is24Bit ? &Z80::A24RegClass
                                                   : &Z80::A16RegClass);
  MI.addOperand(MachineOperand::CreateReg(Reg, true  /*IsDef*/,
                                               true  /*IsImp*/,
                                               false /*IsKill*/,
                                               true  /*IsDead*/));
  DEBUG(MI.dump());
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredSub0(MachineInstr &MI,
                                   MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::Sub024;
  assert((Is24Bit || MI.getOpcode() == Z80::Sub016) && "Unexpected opcode");
  unsigned OpReg = Is24Bit ? Z80::UHL : Z80::HL;
  unsigned ScratchReg = Is24Bit ? Z80::UBC : Z80::BC;
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  DEBUG(BB->dump());
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao), OpReg)
    .addReg(OpReg).addReg(ScratchReg, RegState::Undef);
  BuildMI(*BB, MI, DL, TII->get(Z80::RCF));
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
    .addReg(ScratchReg, RegState::Undef);
  MI.eraseFromParent();
  DEBUG(BB->dump());
  return BB;
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredSub(MachineInstr &MI,
                                  MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::Sub24;
  assert((Is24Bit || MI.getOpcode() == Z80::Sub16) && "Unexpected opcode");
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  DEBUG(BB->dump());
  BuildMI(*BB, MI, DL, TII->get(Z80::RCF));
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
          .addReg(MI.getOperand(0).getReg());
  MI.eraseFromParent();
  DEBUG(BB->dump());
  return BB;
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredCmp(MachineInstr &MI,
                                  MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::Cp24;
  assert((Is24Bit || MI.getOpcode() == Z80::Cp16) && "Unexpected opcode");
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  BuildMI(*BB, MI, DL, TII->get(Z80::RCF));
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
    .addReg(MI.getOperand(0).getReg());
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
          Is24Bit ? Z80::UHL : Z80::HL).addReg(Is24Bit ? Z80::UHL : Z80::HL)
    .addReg(MI.getOperand(0).getReg());
  MI.eraseFromParent();
  DEBUG(BB->dump());
  return BB;
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredCmp0(MachineInstr &MI,
                                   MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::Cp024;
  assert((Is24Bit || MI.getOpcode() == Z80::Cp016) && "Unexpected opcode");
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::ADD24ao : Z80::ADD16ao),
          Is24Bit ? Z80::UHL : Z80::HL).addReg(Is24Bit ? Z80::UHL : Z80::HL)
    .addReg(MI.getOperand(0).getReg());
  BuildMI(*BB, MI, DL, TII->get(Z80::RCF));
  BuildMI(*BB, MI, DL, TII->get(Is24Bit ? Z80::SBC24ar : Z80::SBC16ar))
    .addReg(MI.getOperand(0).getReg());
  MI.eraseFromParent();
  DEBUG(BB->dump());
  return BB;
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredSelect(MachineInstr &MI,
                                     MachineBasicBlock *BB) const {
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  // To "insert" a SELECT_CC instruction, we actually have to insert the
  // diamond control-flow pattern.  The incoming instruction knows the
  // destination vreg to set, the condition code register to branch on, the
  // true/false values to select between, and a branch opcode to use.
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator I = ++BB->getIterator();

  //  thisMBB:
  //  ...
  //   %FalseVal = ...
  //   cmpTY ccX, r1, r2
  //   bCC copy1MBB
  //   fallthrough --> copy0MBB
  MachineBasicBlock *thisMBB = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *copy1MBB = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(I, copy0MBB);
  F->insert(I, copy1MBB);

  // Update machine-CFG edges by transferring all successors of the current
  // block to the new block which will contain the Phi node for the select.
  copy1MBB->splice(copy1MBB->begin(), BB,
                   std::next(MachineBasicBlock::iterator(MI)), BB->end());
  copy1MBB->transferSuccessorsAndUpdatePHIs(BB);
  // Next, add the true and fallthrough blocks as its successors.
  BB->addSuccessor(copy0MBB);
  BB->addSuccessor(copy1MBB);

  BuildMI(BB, DL, TII->get(Z80::JQCC)).addMBB(copy1MBB)
    .addImm(MI.getOperand(3).getImm());

  //  copy0MBB:
  //   %TrueVal = ...
  //   # fallthrough to copy1MBB
  BB = copy0MBB;

  // Update machine-CFG edges
  BB->addSuccessor(copy1MBB);

  //  copy1MBB:
  //   %Result = phi [ %FalseValue, copy0MBB ], [ %TrueValue, thisMBB ]
  //  ...
  BB = copy1MBB;
  BuildMI(*BB, BB->begin(), DL, TII->get(Z80::PHI),
          MI.getOperand(0).getReg())
    .addReg(MI.getOperand(1).getReg()).addMBB(thisMBB)
    .addReg(MI.getOperand(2).getReg()).addMBB(copy0MBB);

  MI.eraseFromParent();   // The pseudo instruction is gone now.
  DEBUG(F->dump());
  return BB;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "Z80GenCallingConv.inc"

CCAssignFn *Z80TargetLowering::getCCAssignFn(CallingConv::ID CallConv) const {
  bool Is24Bit = Subtarget.is24Bit();
  switch (CallConv) {
  default: llvm_unreachable("Unsupported calling convention!");
  case CallingConv::C:
    return Is24Bit ? CC_EZ80_C : CC_Z80_C;
  case CallingConv::Z80_LibCall:
    return CC_Z80_LC_AB;
  case CallingConv::Z80_LibCall_AC:
    return CC_Z80_LC_AC;
  case CallingConv::Z80_LibCall_BC:
    return CC_Z80_LC_BC;
  case CallingConv::Z80_LibCall_C:
    return CC_Z80_LC_C;
  case CallingConv::Z80_LibCall_L:
    return CC_Z80_LC_L;
  }
}
CCAssignFn *Z80TargetLowering::getRetCCAssignFn(CallingConv::ID CallConv) const {
  bool Is24Bit = Subtarget.is24Bit();
  switch (CallConv) {
  default: llvm_unreachable("Unsupported calling convention!");
  case CallingConv::C:
  case CallingConv::Z80_LibCall:
  case CallingConv::Z80_LibCall_AC:
  case CallingConv::Z80_LibCall_BC:
  case CallingConv::Z80_LibCall_C:
    return RetCC_Z80_C;
  case CallingConv::Z80_LibCall_L:
    return RetCC_Z80_LC_L;
  }
}

SDValue Z80TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG                     = CLI.DAG;
  SDLoc &DL                             = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool &IsTailCall                      = CLI.IsTailCall;
  bool IsVarArg                         = CLI.IsVarArg;

  MachineFunction &MF = DAG.getMachineFunction();
  bool Is24Bit        = Subtarget.is24Bit();
  if (MF.getFunction()->getFnAttribute("disable-tail-calls")
      .getValueAsString() == "true")
    IsTailCall = false;

  if (IsTailCall)
    IsTailCall = IsEligibleForTailCallOptimization(
        Callee, CallConv, IsVarArg, CLI.RetTy, Outs, OutVals, Ins, DAG);

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, getCCAssignFn(CallConv));

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getAlignedCallFrameSize();
  MVT PtrVT = getPointerTy(DAG.getDataLayout());

  if (!IsTailCall)
    Chain = DAG.getCALLSEQ_START(Chain,
                                 DAG.getIntPtrConstant(NumBytes, DL, true), DL);

  SmallVector<std::pair<unsigned, SDValue>, 2> RegsToPass;
  SmallVector<SDValue, 14> MemOpChains;
  SDValue StackPtr;
  const TargetRegisterInfo *RegInfo = Subtarget.getRegisterInfo();

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned I = 0, E = ArgLocs.size(); I != E; ++I) {
    CCValAssign &VA = ArgLocs[I];
    SDValue Arg = OutVals[I];

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full: break;
    case CCValAssign::SExt: break;
      Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getBitcast(VA.getLocVT(), Arg);
      break;
    }

    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else if (!IsTailCall) {
      assert(VA.isMemLoc());
      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, DL, Is24Bit ? Z80::SPL : Z80::SPS,
                                      PtrVT);
      MemOpChains.push_back(DAG.getStore(
          Chain, DL, Arg,
          DAG.getMemBasePlusOffset(StackPtr, VA.getLocMemOffset(), DL),
          MachinePointerInfo::getStack(DAG.getMachineFunction(),
                                       VA.getLocMemOffset())));
    }
  }

  // Transform all store nodes into one single node.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  // Build a sequence of copy-to-reg nodes chained together with a token chain
  // and flag operands with copy the outgoing args into registers.
  SDValue InFlag;
  for (unsigned I = 0, E = RegsToPass.size(); I != E; ++I) {
    Chain = DAG.getCopyToReg(Chain, DL, RegsToPass[I].first,
                             RegsToPass[I].second, InFlag);
    InFlag = Chain.getValue(1);
  }
  if (IsTailCall)
    InFlag = SDValue();

  // Returns a chain and a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are known live
  // into the call.
  for (unsigned I = 0, E = RegsToPass.size(); I != E; ++I)
    Ops.push_back(DAG.getRegister(RegsToPass[I].first,
                                  RegsToPass[I].second.getValueType()));

  // Add a register mask operand representing the call-preserved registers.
  const uint32_t *Mask = RegInfo->getCallPreservedMask(MF, CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");

  Ops.push_back(DAG.getRegisterMask(Mask));
  if (InFlag.getNode())
    Ops.push_back(InFlag);

  if (IsTailCall) {
    MF.getFrameInfo().setHasTailCall();
    return DAG.getNode(Z80ISD::TC_RETURN, DL, NodeTys, Ops);
  }

  // Returns a chain and a flag for retval copy to use.
  Chain = DAG.getNode(Z80ISD::CALL, DL, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, DL, true),
                             DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
  InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, IsVarArg, Ins, DL, DAG,
                         InVals);
}

/// MatchingStackOffset - Return true if the given stack call argument is
/// already available in the same position (relatively) of the caller's
/// incoming argument stack.
static bool MatchingStackOffset(SDValue Arg, unsigned Offset,
                                ISD::ArgFlagsTy Flags, MachineFrameInfo &MFI,
                                const MachineRegisterInfo *MRI,
                                const TargetInstrInfo *TII) {
  unsigned Bytes = Arg.getValueType().getSizeInBits() / 8;
  int FI = INT_MAX;
  if (Arg.getOpcode() == ISD::CopyFromReg) {
    unsigned VR = cast<RegisterSDNode>(Arg.getOperand(1))->getReg();
    if (!TargetRegisterInfo::isVirtualRegister(VR))
      return false;
    MachineInstr *Def = MRI->getVRegDef(VR);
    if (!Def)
      return false;
    if (Flags.isByVal() || !TII->isLoadFromStackSlot(*Def, FI))
      return false;
  } else if (LoadSDNode *Ld = dyn_cast<LoadSDNode>(Arg)) {
    if (Flags.isByVal())
      return false;
    SDValue Ptr = Ld->getBasePtr();
    FrameIndexSDNode *FINode = dyn_cast<FrameIndexSDNode>(Ptr);
    if (!FINode)
      return false;
    FI = FINode->getIndex();
  } else if (Arg.getOpcode() == ISD::FrameIndex && Flags.isByVal()) {
    FrameIndexSDNode *FINode = cast<FrameIndexSDNode>(Arg);
    FI = FINode->getIndex();
    Bytes = Flags.getByValSize();
  } else
    return false;

  assert(FI != INT_MAX);
  return MFI.isFixedObjectIndex(FI) && Offset == MFI.getObjectOffset(FI) &&
    Bytes == MFI.getObjectSize(FI);
}

/// Check whether the call is eligible for tail call optimization. Targets
/// that want to do tail call optimization should implement this function.
bool Z80TargetLowering::IsEligibleForTailCallOptimization(
    SDValue Callee, CallingConv::ID CalleeCC, bool isVarArg, Type *RetTy,
    const SmallVectorImpl<ISD::OutputArg> &Outs,
    const SmallVectorImpl<SDValue> &OutVals,
    const SmallVectorImpl<ISD::InputArg> &Ins, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  const Function *CallerF = MF.getFunction();
  CallingConv::ID CallerCC = CallerF->getCallingConv();
  // TODO: Handle other calling conventions when they exist
  if (CalleeCC != CallingConv::C || CallerCC != CallingConv::C)
    return false;
  LLVMContext &C = *DAG.getContext();
  if (!CCState::resultsCompatible(CalleeCC, CallerCC, MF, C, Ins,
                                  RetCC_Z80_C, RetCC_Z80_C))
    return false;
  // If the callee takes no arguments then go on to check the results of the
  // call.
  if (!Outs.empty()) {
    // Check if stack adjustment is needed. For now, do not do this if any
    // argument is passed on the stack.
    SmallVector<CCValAssign, 16> ArgLocs;
    CCState CCInfo(CalleeCC, isVarArg, MF, ArgLocs, C);
    CCInfo.AnalyzeCallOperands(Outs, getCCAssignFn(CalleeCC));
    if (CCInfo.getNextStackOffset()) {
      // Check if the arguments are already laid out in the right way as
      // the caller's fixed stack objects.
      MachineFrameInfo &MFI = MF.getFrameInfo();
      const MachineRegisterInfo *MRI = &MF.getRegInfo();
      const TargetInstrInfo *TII = Subtarget.getInstrInfo();
      for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
        CCValAssign &VA = ArgLocs[i];
        SDValue Arg = OutVals[i];
        ISD::ArgFlagsTy Flags = Outs[i].Flags;
        if (VA.getLocInfo() == CCValAssign::Indirect)
          return false;
        if (!VA.isRegLoc() && !MatchingStackOffset(Arg, VA.getLocMemOffset(),
                                                   Flags, MFI, MRI, TII/*, VA*/))
          return false;
      }
    }
  }
  return true;
}

SDValue Z80TargetLowering::LowerReturn(SDValue Chain,
                                       CallingConv::ID CallConv, bool IsVarArg,
                                       const SmallVectorImpl<ISD::OutputArg> &Outs,
                                       const SmallVectorImpl<SDValue> &OutVals,
                                       const SDLoc &DL, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();

  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, getRetCCAssignFn(CallConv));

  SDValue Flag;
  SmallVector<SDValue, 6> RetOps;
  RetOps.push_back(Chain);

  // Copy the result values into the output registers.
  for (unsigned I = 0, E = RVLocs.size(); I != E; ++I) {
    CCValAssign &VA = RVLocs[I];

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[I], Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain; // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(Z80ISD::RET_FLAG, DL, MVT::Other, RetOps);
}

/// Lower the result values of a call into the appropriate copies out of
/// appropriate physical registers.
///
SDValue
Z80TargetLowering::LowerCallResult(SDValue Chain, SDValue InFlag,
                                   CallingConv::ID CallConv, bool IsVarArg,
                                   const SmallVectorImpl<ISD::InputArg> &Ins,
                                   SDLoc DL, SelectionDAG &DAG,
                                   SmallVectorImpl<SDValue> &InVals) const {
  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeCallResult(Ins, getRetCCAssignFn(CallConv));

  // Copy all of the result registers out of their specified physreg.
  for (unsigned I = 0, E = RVLocs.size(); I != E; ++I) {
    Chain = DAG.getCopyFromReg(Chain, DL, RVLocs[I].getLocReg(),
                               RVLocs[I].getValVT(), InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

EVT Z80TargetLowering::getTypeForExtReturn(LLVMContext &Context, EVT VT,
                                           ISD::NodeType ExtendKind) const {
  // The ABI does not require anything to be extended.
  return VT;
}

SDValue Z80TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  bool Is24Bit = Subtarget.is24Bit();

  assert(CallConv == CallingConv::C && "Unsupported calling convention");
  assert(!IsVarArg && "Var args not supported yet");

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, Is24Bit ? CC_EZ80_C : CC_Z80_C);

  SDValue ArgValue;
  for (unsigned I = 0, E = ArgLocs.size(); I != E; ++I) {
    CCValAssign &VA = ArgLocs[I];
    assert(VA.isMemLoc() && "Don't support register passed arguments yet");
    int FI = MFI.CreateFixedObject(VA.getValVT().getSizeInBits()/8,
                                   VA.getLocMemOffset(), false);
    SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue Val = DAG.getLoad(
        VA.getValVT(), DL, Chain, FIN,
        MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
    InVals.push_back(Val);
  }

  return Chain;
}

const char *Z80TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((Z80ISD::NodeType)Opcode) {
  case Z80ISD::FIRST_NUMBER: break;
  case Z80ISD::Wrapper:      return "Z80ISD::Wrapper";
  case Z80ISD::RLC:          return "Z80ISD::RLC";
  case Z80ISD::RRC:          return "Z80ISD::RRC";
  case Z80ISD::RL:           return "Z80ISD::RL";
  case Z80ISD::RR:           return "Z80ISD::RR";
  case Z80ISD::SLA:          return "Z80ISD::SLA";
  case Z80ISD::SRA:          return "Z80ISD::SRA";
  case Z80ISD::SRL:          return "Z80ISD::SRL";
  case Z80ISD::INC:          return "Z80ISD::INC";
  case Z80ISD::DEC:          return "Z80ISD::DEC";
  case Z80ISD::ADD:          return "Z80ISD::ADD";
  case Z80ISD::ADC:          return "Z80ISD::ADC";
  case Z80ISD::SUB:          return "Z80ISD::SUB";
  case Z80ISD::SBC:          return "Z80ISD::SBC";
  case Z80ISD::AND:          return "Z80ISD::AND";
  case Z80ISD::XOR:          return "Z80ISD::XOR";
  case Z80ISD::OR:           return "Z80ISD::OR";
  case Z80ISD::CP:           return "Z80ISD::CP";
  case Z80ISD::TST:          return "Z80ISD::TST";
  case Z80ISD::MLT:          return "Z80ISD::MLT";
  case Z80ISD::CALL:         return "Z80ISD::CALL";
  case Z80ISD::RET_FLAG:     return "Z80ISD::RET_FLAG";
  case Z80ISD::TC_RETURN:    return "Z80ISD::TC_RETURN";
  case Z80ISD::BRCOND:       return "Z80ISD::BRCOND";
  case Z80ISD::SELECT:       return "Z80ISD::SELECT";
  }
  return nullptr;
}

EVT Z80TargetLowering::getSetCCResultType(const DataLayout &DL,
                                          LLVMContext &Context,
                                          EVT VT) const {
  assert(!VT.isVector() && "No default SetCC type for vectors!");
  return MVT::i1;
}
