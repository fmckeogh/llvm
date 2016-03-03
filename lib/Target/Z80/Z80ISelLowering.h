//===-- Z80ISelLowering.h - Z80 DAG Lowering Interface ----------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_Z80_Z80ISELLOWERING_H
#define LLVM_LIB_TARGET_Z80_Z80ISELLOWERING_H

#include "llvm/Target/TargetLowering.h"

namespace llvm {
class Z80Subtarget;
class Z80TargetMachine;

namespace Z80ISD {
  // Z80 Specific DAG Nodes
  enum NodeType : unsigned {
    // Start the numbering where the builtin ops leave off.
    FIRST_NUMBER = ISD::BUILTIN_OP_END,

    /// Return with a flag operand. Operand 0 is the chain operand, operand
    /// 1 is the number of bytes of stack to pop.
    RET_FLAG
  };
}

//===----------------------------------------------------------------------===//
//  Z80 Implementation of the TargetLowering interface
class Z80TargetLowering final : public TargetLowering {
    /// Keep a reference to the Z80Subtarget around so that we can
    /// make the right decision when generating code for different targets.
    const Z80Subtarget &Subtarget;

public:
  explicit Z80TargetLowering(const Z80TargetMachine &TM,
                             const Z80Subtarget &STI);

  /// This method returs the name of a target specific DAG node.
  const char *getTargetNodeName(unsigned Opcode) const override;

private:
  SDValue LowerFormalArguments(SDValue Chain,
                               CallingConv::ID CallConv, bool isVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               SDLoc DL, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerReturn(SDValue Chain,
                      CallingConv::ID CallConv, bool isVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutVals,
                      SDLoc DL, SelectionDAG &DAG) const override;

  EVT getTypeForExtReturn(LLVMContext &Context, EVT VT,
                          ISD::NodeType ExtendKind) const override;
};
} // End llvm namespace

#endif
