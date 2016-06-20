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
  }
  if (Subtarget.hasEZ80Ops())
    setOperationAction(ISD::MUL, MVT::i8, Custom);

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

  setJumpIsExpensive();
  setSelectIsExpensive();

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
  assert(Op.getSimpleValueType() == MVT::i32 &&
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

SDValue Z80TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::ADD:  case ISD::SUB:
  case ISD::ADDC: case ISD::SUBC:
  case ISD::ADDE: case ISD::SUBE: return LowerADDSUB(Op, DAG);
  case ISD::SHL: return LowerSHL(Op, DAG);
  case ISD::SRA: return LowerSHR(true, Op, DAG);
  case ISD::SRL: return LowerSHR(false, Op, DAG);
  case ISD::MUL: return LowerMUL(Op, DAG);
  }
  if (Op.getValueSizeInBits() == 16) {
    switch (Op.getOpcode()) {
      default: llvm_unreachable("Should not custom lower this!");
      case ISD::AND: // Fallthrough
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
  assert(Op.getSimpleValueType() == MVT::i16 &&
         "Can only narrow i16 operations");
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
  case ISD::AND:
  case ISD::XOR:
  case ISD:: OR: return;
  }
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

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, getCCAssignFn(CallConv));

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getAlignedCallFrameSize();
  MVT PtrVT = getPointerTy(DAG.getDataLayout());

  Chain = DAG.getCALLSEQ_START(Chain, DAG.getIntPtrConstant(NumBytes, DL, true),
                               DL);

  SmallVector<std::pair<unsigned, SDValue>, 2> RegsToPass;
  SmallVector<SDValue, 14> MemOpChains;
  SDValue StackPtr;

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
    } else {
      assert(VA.isMemLoc());
      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, DL, Is24Bit ? Z80::SPL : Z80::SPS,
                                      PtrVT);
      SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset(), DL);
      PtrOff = DAG.getNode(ISD::ADD, DL, PtrVT, StackPtr, PtrOff);
      MemOpChains.push_back(DAG.getStore(
          Chain, DL, Arg, PtrOff,
          MachinePointerInfo::getStack(DAG.getMachineFunction(),
                                       VA.getLocMemOffset()), false, false, 0));
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

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(GA->getGlobal(), DL, PtrVT);
  else if (ExternalSymbolSDNode *ES = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(ES->getSymbol(), PtrVT);

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

  // Returns a chain and a flag for retval copy to use.
  Chain = DAG.getNode(Z80ISD::CALL, DL, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, DL, true),
                             DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
  InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, IsVarArg,
                         Ins, DL, DAG, InVals);
}

SDValue Z80TargetLowering::LowerReturn(SDValue Chain,
                                       CallingConv::ID CallConv, bool IsVarArg,
                                       const SmallVectorImpl<ISD::OutputArg> &Outs,
                                       const SmallVectorImpl<SDValue> &OutVals,
                                       SDLoc DL, SelectionDAG &DAG) const {
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
    const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc DL, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
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
    int FI = MFI->CreateFixedObject(VA.getValVT().getSizeInBits()/8,
                                    VA.getLocMemOffset(), false);
    SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue Val = DAG.getLoad(
        VA.getValVT(), DL, Chain, FIN,
        MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI), false,
        false, false, 0);
    InVals.push_back(Val);
  }

  return Chain;
}

const char *Z80TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((Z80ISD::NodeType)Opcode) {
  case Z80ISD::FIRST_NUMBER: break;
  case Z80ISD::MLT:          return "Z80ISD::MLT";
  case Z80ISD::CALL:         return "Z80ISD::CALL";
  case Z80ISD::RET_FLAG:     return "Z80ISD::RETFLAG";
  }
  return nullptr;
}
