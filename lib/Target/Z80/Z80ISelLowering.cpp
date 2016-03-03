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
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/SelectionDAG.h"
using namespace llvm;

#define DEBUG_TYPE "z80-isel"

Z80TargetLowering::Z80TargetLowering(const Z80TargetMachine &TM,
                                     const Z80Subtarget &STI)
    : TargetLowering(TM), Subtarget(STI) {
  // Set up the register classes.
  addRegisterClass(MVT::i8, &Z80::G8RegClass);
  addRegisterClass(MVT::i16, &Z80::G16RegClass);
  if (Subtarget.is24Bit())
    addRegisterClass(MVT::i24, &Z80::G24RegClass);

  // Compute derived properties from the register classes
  computeRegisterProperties(STI.getRegisterInfo());
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "Z80GenCallingConv.inc"

SDValue Z80TargetLowering::LowerReturn(SDValue Chain,
                                       CallingConv::ID CallConv, bool isVarArg,
                                       const SmallVectorImpl<ISD::OutputArg> &Outs,
                                       const SmallVectorImpl<SDValue> &OutVals,
                                       SDLoc DL, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  bool Is24Bit = Subtarget.is24Bit();

  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, MF, RVLocs, *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, Is24Bit ? RetCC_EZ80_C : RetCC_Z80_C);

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

EVT Z80TargetLowering::getTypeForExtReturn(LLVMContext &Context, EVT VT,
                                           ISD::NodeType ExtendKind) const {
  EVT MinVT = getRegisterType(Context, MVT::i8);
  return VT.bitsLT(MinVT) ? MinVT : VT;
}

SDValue Z80TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc DL, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  bool Is24Bit = Subtarget.is24Bit();

  assert(CallConv == CallingConv::C && "Unsupported calling convention");
  assert(!isVarArg && "Var args not supported yet");

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, MF, ArgLocs, *DAG.getContext());
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
  case Z80ISD::RET_FLAG:     return "Z80ISD::RETFLAG";
  }
  return nullptr;
}
