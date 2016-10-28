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
  setSchedulingPreference(Sched::RegPressure);

  // Set up the register classes.
  addRegisterClass(MVT::i8, &Z80::R8RegClass);
  addRegisterClass(MVT::i16, &Z80::R16RegClass);
  if (Subtarget.is24Bit()) {
    addRegisterClass(MVT::i24, &Z80::R24RegClass);
    addRegisterClass(MVT::i32, &Z80::R32RegClass);
    for (unsigned Opc : { ISD::ADD, ISD::SUB,
                          ISD::ADDC, ISD::SUBC,
                          ISD::ADDE, ISD::SUBE })
      setOperationAction(Opc, MVT::i32, Custom);
  }
  for (auto VT : { MVT::i16, MVT::i24, MVT::i32 })
    for (unsigned Opc : { ISD::AND, ISD::OR, ISD::XOR, ISD::MUL })
      setOperationAction(Opc, VT, LibCall);
  for (auto VT : { MVT::i8, MVT::i16, MVT::i24, MVT::i32 }) {
    for (unsigned Opc : { ISD::SHL, ISD::SRA, ISD::SRL })
      setOperationAction(Opc, VT, Custom);
    for (unsigned Opc : { ISD::MUL,
                          ISD::SDIV,    ISD::UDIV,
                          ISD::SREM,    ISD::UREM,
                          ISD::SDIVREM, ISD::UDIVREM })
      setOperationAction(Opc, VT, LibCall);
    setOperationAction(ISD::BR_CC, VT, Custom);
    setOperationAction(ISD::SETCC, VT, Custom);
    setOperationAction(ISD::SELECT, VT, Expand);
    setOperationAction(ISD::SELECT_CC, VT, Custom);
  }
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  if (Subtarget.hasEZ80Ops())
    setOperationAction(ISD::MUL, MVT::i8, Custom);

  if (!Subtarget.hasEZ80Ops()) {
    setOperationAction(ISD::LOAD, MVT::i16, Custom);
    setOperationAction(ISD::STORE, MVT::i16, Custom);
  }
  if (Subtarget.is24Bit()) {
    setOperationAction(ISD::LOAD, MVT::i32, Custom);
    setOperationAction(ISD::STORE, MVT::i32, Custom);
  }
  for (MVT ValVT : MVT::integer_valuetypes()) {
    for (MVT MemVT : MVT::integer_valuetypes()) {
      setLoadExtAction(ISD:: EXTLOAD, ValVT, MemVT, Expand);
      setLoadExtAction(ISD::ZEXTLOAD, ValVT, MemVT, Expand);
      setLoadExtAction(ISD::SEXTLOAD, ValVT, MemVT, Expand);
      setTruncStoreAction(ValVT, MemVT, Expand);
    }
  }

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
  setLibcallCallingConv(RTLIB::SHL_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I16_I8, "_sshl_b");
  setLibcallCallingConv(RTLIB::SHL_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I24, "_ishl");
  setLibcallCallingConv(RTLIB::SHL_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I24_I8, "_ishl_b");
  setLibcallCallingConv(RTLIB::SHL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SHL_I32, "_lshl");
  setLibcallCallingConv(RTLIB::SHL_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I8, "_bshrs");
  setLibcallCallingConv(RTLIB::SRA_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I16, "_sshrs");
  setLibcallCallingConv(RTLIB::SRA_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I16_I8, "_sshrs_b");
  setLibcallCallingConv(RTLIB::SRA_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I24, "_ishrs");
  setLibcallCallingConv(RTLIB::SRA_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I24_I8, "_ishrs_b");
  setLibcallCallingConv(RTLIB::SRA_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRA_I32, "_lshrs");
  setLibcallCallingConv(RTLIB::SRA_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I8, "_bshl");
  setLibcallCallingConv(RTLIB::SRL_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I16, "_sshru");
  setLibcallCallingConv(RTLIB::SRL_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I16_I8, "_sshru_b");
  setLibcallCallingConv(RTLIB::SRL_I16_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I24, "_ishru");
  setLibcallCallingConv(RTLIB::SRL_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I24_I8, "_ishru_b");
  setLibcallCallingConv(RTLIB::SRL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SRL_I32, "_lshru");
  setLibcallCallingConv(RTLIB::SRL_I32, CallingConv::Z80_LibCall);
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
  setLibcallCallingConv(RTLIB::MUL_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I16, "_smulu");
  setLibcallCallingConv(RTLIB::MUL_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I24, "_imulu");
  setLibcallCallingConv(RTLIB::MUL_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I24_I8, "_imul_b");
  setLibcallCallingConv(RTLIB::MUL_I24_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::MUL_I32, "_lmulu");
  setLibcallCallingConv(RTLIB::MUL_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I8, "_bdivs");
  setLibcallCallingConv(RTLIB::SDIV_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I16, "_sdivs");
  setLibcallCallingConv(RTLIB::SDIV_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I24, "_idivs");
  setLibcallCallingConv(RTLIB::SDIV_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SDIV_I32, "_ldivs");
  setLibcallCallingConv(RTLIB::SDIV_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I8, "_bdivu");
  setLibcallCallingConv(RTLIB::UDIV_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I16, "_sdivu");
  setLibcallCallingConv(RTLIB::UDIV_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I24, "_idivu");
  setLibcallCallingConv(RTLIB::UDIV_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UDIV_I32, "_ldivu");
  setLibcallCallingConv(RTLIB::UDIV_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I8, "_brems");
  setLibcallCallingConv(RTLIB::SREM_I8, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I16, "_srems");
  setLibcallCallingConv(RTLIB::SREM_I16, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I24, "_irems");
  setLibcallCallingConv(RTLIB::SREM_I24, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::SREM_I32, "_lrems");
  setLibcallCallingConv(RTLIB::SREM_I32, CallingConv::Z80_LibCall);
  setLibcallName(RTLIB::UREM_I8, "_bremu");
  setLibcallCallingConv(RTLIB::UREM_I8, CallingConv::Z80_LibCall);
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

static SDValue LowerADDSUB(SDValue Op, SelectionDAG &DAG) {
  /*if (Op.getValueType() != MVT::i32) {
    switch (Op.getOpcode()) {
    default: llvm_unreachable("Can only handle adds/subs");
    case ISD::ADD:
    case ISD::SUB:
    }
    }*/
  assert(Op.getValueType() == MVT::i32 &&
         "Can only handle 32-bit operations");
  unsigned OpC, OpE;
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Can only handle adds/subs");
  case ISD::ADD: case ISD::ADDC: OpC = ISD::ADDC; OpE = ISD::ADDE; break;
  case ISD::ADDE: OpC = OpE = ISD::ADDE; break;
  case ISD::SUB: case ISD::SUBC: OpC = ISD::SUBC; OpE = ISD::SUBE; break;
  case ISD::SUBE: OpC = OpE = ISD::ADDE; break;
  }
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);
  SDValue LH = DAG.getTargetExtractSubreg(Z80::sub_top,  DL, MVT::i8,  L);
  SDValue LL = DAG.getTargetExtractSubreg(Z80::sub_long, DL, MVT::i24, L);
  SDValue RH = DAG.getTargetExtractSubreg(Z80::sub_top,  DL, MVT::i8,  R);
  SDValue RL = DAG.getTargetExtractSubreg(Z80::sub_long, DL, MVT::i24, R);
  SDValue Ops[3] = { LL, RL };
  size_t OpCount = 2;
  if (OpC == OpE)
    Ops[OpCount++] = Op.getOperand(2);
  SDValue Lo = DAG.getNode(OpC, DL, DAG.getVTList(MVT::i24, MVT::Glue),
                           {Ops, OpCount});
  SDValue Hi = DAG.getNode(OpE, DL, DAG.getVTList(MVT::i8,  MVT::Glue),
                           LL, RL, Lo.getValue(1));
  SDValue Result = DAG.getUNDEF(MVT::i16);
  Result = DAG.getTargetInsertSubreg(Z80::sub_long, DL, MVT::i32, Result, Lo);
  Result = DAG.getTargetInsertSubreg(Z80::sub_top,  DL, MVT::i32, Result, Hi);
  Ops[0] = Result;
  Ops[1] = Hi.getValue(1);
  return DAG.getMergeValues({Ops, 2}, DL);
}

static SDValue LowerSHL(SDValue Op, SelectionDAG &DAG) {
  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  if (ConstantSDNode *ShiftAmountNode =
      dyn_cast<ConstantSDNode>(Op.getOperand(1))) {
    uint64_t ShiftAmount = ShiftAmountNode->getZExtValue();
    SDValue Res = Op.getOperand(0);
    while (ShiftAmount--)
      Res = DAG.getNode(ISD::ADD, DL, VT, Res, Res);
    return Res;
  } else {
    llvm_unreachable("Unimplemented");
  }
}
static SDValue LowerSHR(bool Signed, SDValue Op, SelectionDAG &DAG) {
  llvm_unreachable("Unimplemented");
}

static SDValue LowerMUL(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Result = DAG.getUNDEF(MVT::i16);
  Result = DAG.getTargetInsertSubreg(Z80::sub_low,  DL, MVT::i16, Result,
                                     Op.getOperand(0));
  Result = DAG.getTargetInsertSubreg(Z80::sub_high, DL, MVT::i16, Result,
                                     Op.getOperand(1));
  Result = DAG.getNode(Z80ISD::MLT, DL, MVT::i16, Result);
  return DAG.getTargetExtractSubreg(Z80::sub_low, DL, MVT::i8, Result);
}

SDValue Z80TargetLowering::EmitCMP(SDValue &LHS, SDValue &RHS, SDValue &TargetCC,
                ISD::CondCode CC, const SDLoc &DL, SelectionDAG &DAG) const {
  assert(!LHS.getValueType().isFloatingPoint() && "We don't handle FP yet");

  Z80::CondCode TCC = Z80::COND_INVALID;
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
  case ISD::SETULE:
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETUGE:
    // Turn lhs u>= rhs with lhs constant into rhs u< lhs+1, this allows us to
    // fold constant into instruction.
    if (const ConstantSDNode *C = dyn_cast<ConstantSDNode>(LHS)) {
      LHS = RHS;
      RHS = DAG.getConstant(C->getSExtValue() + 1, DL, C->getValueType(0));
      TCC = Z80::COND_C;
      break;
    }
    TCC = Z80::COND_NC;
    break;
  case ISD::SETUGT:
    std::swap(LHS, RHS);
    LLVM_FALLTHROUGH;
  case ISD::SETULT:
    // Turn lhs u< rhs with lhs constant into rhs u>= lhs+1, this allows us to
    // fold constant into instruction.
    if (const ConstantSDNode * C = dyn_cast<ConstantSDNode>(LHS)) {
      LHS = RHS;
      RHS = DAG.getConstant(C->getSExtValue() + 1, DL, C->getValueType(0));
      TCC = Z80::COND_NC;
      break;
    }
    TCC = Z80::COND_C;
    break;
  }

  TargetCC = DAG.getConstant(TCC, DL, MVT::i8);
  return DAG.getNode(Z80ISD::CMP, DL, MVT::Glue, LHS, RHS);
}

SDValue Z80TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS   = Op.getOperand(2);
  SDValue RHS   = Op.getOperand(3);
  SDValue Dest  = Op.getOperand(4);
  SDLoc DL(Op);

  SDValue TargetCC;
  SDValue Flag = EmitCMP(LHS, RHS, TargetCC, CC, DL, DAG);

  return DAG.getNode(Z80ISD::BRCOND, DL, Op.getValueType(),
                     Chain, Dest, TargetCC, Flag);
}

SDValue Z80TargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDLoc DL(Op);

  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  SDValue TargetCC;
  SDValue Flag = EmitCMP(LHS, RHS, TargetCC, CC, DL, DAG);

  EVT VT = Op.getValueType();
  return DAG.getNode(Z80ISD::SELECT, DL, DAG.getVTList(VT, MVT::Glue),
                     DAG.getConstant(1, DL, VT), DAG.getConstant(0, DL, VT),
                     TargetCC, Flag);
}

SDValue Z80TargetLowering::LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TV  = Op.getOperand(2);
  SDValue FV  = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDLoc DL(Op);

  SDValue TargetCC;
  SDValue Flag = EmitCMP(LHS, RHS, TargetCC, CC, DL, DAG);

  return DAG.getNode(Z80ISD::SELECT, DL,
                     DAG.getVTList(Op.getValueType(), MVT::Glue), TV, FV,
                     TargetCC, Flag);
}

SDValue Z80TargetLowering::LowerLOAD(LoadSDNode *Node, SelectionDAG &DAG) const {
  SDLoc DL(Node);
  SDValue Ch = Node->getChain();
  SDValue Ptr = Node->getBasePtr();
  const MachinePointerInfo &MPI = Node->getPointerInfo();
  unsigned Alignment = Node->getAlignment();
  MachineMemOperand *MMO = Node->getMemOperand();
  AAMDNodes AAInfo = Node->getAAInfo();
  unsigned RC, LoTy, LoIdx, HiTy, HiIdx, HiOff;
  bool Split = Z80::splitReg(MMO->getSize(), MVT::i8, MVT::i16, MVT::i24,
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff, Subtarget);
  assert(Split && "Can only custom lower splittable loads");
  SDValue Lo = DAG.getLoad(MVT::SimpleValueType(LoTy), DL, Ch, Ptr, MPI,
                           Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getNode(ISD::ADD, DL, Ptr.getValueType(), Ptr,
                    DAG.getConstant(HiOff, DL, Ptr.getValueType()));
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
SDValue Z80TargetLowering::LowerSTORE(StoreSDNode *Node, SelectionDAG &DAG) const {
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
                             RC, LoTy, LoIdx, HiTy, HiIdx, HiOff, Subtarget);
  assert(Split && "Can only custom lower splittable stores");
  SDValue Lo = DAG.getTargetExtractSubreg(LoIdx, DL, MVT::SimpleValueType(LoTy),
                                          Val);
  Lo = DAG.getStore(Ch, DL, Lo, Ptr, MPI, Alignment, MMO->getFlags(), AAInfo);
  Ptr = DAG.getNode(ISD::ADD, DL, Ptr.getValueType(), Ptr,
                    DAG.getConstant(HiOff, DL, Ptr.getValueType()));
  SDValue Hi = DAG.getTargetExtractSubreg(HiIdx, DL, MVT::SimpleValueType(HiTy),
                                          Val);
  Hi = DAG.getStore(Ch, DL, Hi, Ptr, MPI.getWithOffset(HiOff),
                    MinAlign(Alignment, HiOff), MMO->getFlags(), AAInfo);
  Ch = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                   Lo.getValue(0), Hi.getValue(0));
  return Ch;
}

SDValue Z80TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::ADD: case ISD::SUB:
  case ISD::ADDC: case ISD::SUBC:
  case ISD::ADDE: case ISD::SUBE: return LowerADDSUB(Op, DAG);
  case ISD::SHL: return LowerSHL(Op, DAG);
  case ISD::SRA: return LowerSHR(true, Op, DAG);
  case ISD::SRL: return LowerSHR(false, Op, DAG);
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
      case ISD::AND: return LowerLibCall(RTLIB::AND_I24, RTLIB::AND_I16, Op, DAG);
      case ISD::XOR: return LowerLibCall(RTLIB::XOR_I24, RTLIB::XOR_I16, Op, DAG);
      case ISD:: OR: return LowerLibCall(RTLIB:: OR_I24, RTLIB:: OR_I16, Op, DAG);
    }
  }
}

SDValue Z80TargetLowering::NarrowOperation(SDValue Op, SelectionDAG &DAG) const {
  assert(Op.getValueType() == MVT::i16 && "Can only narrow i16 operations");
  SDLoc DL(Op);
  SDValue L = Op.getOperand(0);
  SDValue R = Op.getOperand(1);
  SDValue LH = DAG.getTargetExtractSubreg(Z80::sub_high, DL, MVT::i8, L);
  SDValue LL = DAG.getTargetExtractSubreg(Z80::sub_low,  DL, MVT::i8, L);
  SDValue RH = DAG.getTargetExtractSubreg(Z80::sub_high, DL, MVT::i8, R);
  SDValue RL = DAG.getTargetExtractSubreg(Z80::sub_low,  DL, MVT::i8, R);
  SDValue Lo = DAG.getNode(Op.getOpcode(), DL, MVT::i8, LL, RL);
  SDValue Result = DAG.getUNDEF(MVT::i16);
  SDValue Hi = DAG.getNode(Op.getOpcode(), DL, MVT::i8, LH, RH);
  Result = DAG.getTargetInsertSubreg(Z80::sub_low,  DL, MVT::i16, Result, Lo);
  Result = DAG.getTargetInsertSubreg(Z80::sub_high, DL, MVT::i16, Result, Hi);
  return Result;
}

SDValue Z80TargetLowering::LowerLibCall(RTLIB::Libcall LC24,
                                        RTLIB::Libcall LC32,
                                        SDValue Op, SelectionDAG &DAG) const {
  RTLIB::Libcall LC;
  if (Op.getValueSizeInBits() == 24) {
    assert(Op.getSimpleValueType() == MVT::i24 && "Can not lower this type");
    LC = LC24;
  } else {
    assert(Op.getSimpleValueType() == MVT::i32 && "Can not lower this type");
    LC = LC32;
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
void Z80TargetLowering::ReplaceNodeResults(SDNode *N,
                                           SmallVectorImpl<SDValue> &Results,
                                           SelectionDAG &DAG) const {
  SDLoc DL(N);
  switch (N->getOpcode()) {
  default:
    llvm_unreachable("Don't know how to custom type legalize this operation!");
  //case ISD::AND:
  //case ISD::XOR:
  //case ISD:: OR: return;
  }
}

MachineBasicBlock *
Z80TargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                               MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unexpected instr type to insert");
  case Z80::ADJCALLSTACKUP16:
  case Z80::ADJCALLSTACKUP24:
  case Z80::ADJCALLSTACKDOWN16:
  case Z80::ADJCALLSTACKDOWN24:
    return EmitAdjCallStack(MI, BB);
  case Z80::Sub16:
  case Z80::Sub24:
    return EmitLoweredSub(MI, BB);
  case Z80::Cmp16:
  case Z80::Cmp24:
    return EmitLoweredCmp(MI, BB);
  case Z80::Select8:
  case Z80::Select16:
  case Z80::Select24:
    return EmitLoweredSelect(MI, BB);
  }
}

MachineBasicBlock *
Z80TargetLowering::EmitAdjCallStack(MachineInstr &MI,
                                    MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::ADJCALLSTACKUP24 ||
                 MI.getOpcode() == Z80::ADJCALLSTACKDOWN24;
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  MachineRegisterInfo &MRI = BB->getParent()->getRegInfo();
  unsigned Reg = MRI.createVirtualRegister(Is24Bit ? &Z80::A24RegClass
                                                   : &Z80::A16RegClass);
  MI.addOperand(MachineOperand::CreateReg(Reg, true  /*IsDef*/,
                                               false /*IsImp*/,
                                               false /*IsKill*/,
                                               true  /*IsDead*/));
  DEBUG(MI.dump());
  return BB;
}

MachineBasicBlock *
Z80TargetLowering::EmitLoweredSub(MachineInstr &MI,
                                  MachineBasicBlock *BB) const {
  bool Is24Bit = MI.getOpcode() == Z80::Sub24;
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
  bool Is24Bit = MI.getOpcode() == Z80::Cmp24;
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  DEBUG(BB->dump());
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
    return CC_Z80_LC;
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
      SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset(), DL);
      PtrOff = DAG.getNode(ISD::ADD, DL, PtrVT, StackPtr, PtrOff);
      MemOpChains.push_back(DAG.getStore(
          Chain, DL, Arg, PtrOff,
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

  // Handle result values, copying them out of physregs into vregs that we
  // return.  This is done before CALLSEQ_END because that may clobber the
  // result registers.
  Chain = LowerCallResult(Chain, InFlag, CallConv, IsVarArg,
                          Ins, DL, DAG, InVals);
  InFlag = Chain.getValue(2);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, DL, true),
                             DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
  InFlag = Chain.getValue(1);

  return Chain;
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
  CCInfo.AnalyzeReturn(Outs, RetCC_Z80_C);

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
  CCInfo.AnalyzeCallResult(Ins, RetCC_Z80_C);

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
  EVT MinVT = getRegisterType(Context, MVT::i8);
  return VT.bitsLT(MinVT) ? MinVT : VT;
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
  case Z80ISD::INC:          return "Z80ISD::INC";
  case Z80ISD::DEC:          return "Z80ISD::DEC";
  case Z80ISD::ADD:          return "Z80ISD::ADD";
  case Z80ISD::ADC:          return "Z80ISD::ADC";
  case Z80ISD::SUB:          return "Z80ISD::SUB";
  case Z80ISD::SBC:          return "Z80ISD::SBC";
  case Z80ISD::AND:          return "Z80ISD::AND";
  case Z80ISD::XOR:          return "Z80ISD::XOR";
  case Z80ISD::OR:           return "Z80ISD::OR";
  case Z80ISD::MLT:          return "Z80ISD::MLT";
  case Z80ISD::CALL:         return "Z80ISD::CALL";
  case Z80ISD::RET_FLAG:     return "Z80ISD::RET_FLAG";
  case Z80ISD::TC_RETURN:    return "Z80ISD::TC_RETURN";
  case Z80ISD::CMP:          return "Z80ISD::CMP";
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
