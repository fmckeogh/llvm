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
#include "llvm/CodeGen/CallingConvLower.h"

namespace llvm {
class Z80Subtarget;
class Z80TargetMachine;

namespace Z80ISD {
  // Z80 Specific DAG Nodes
  enum NodeType : unsigned {
    // Start the numbering where the builtin ops leave off.
    FIRST_NUMBER = ISD::BUILTIN_OP_END,

    MLT,

    /// This operation represents an abstract Z80 call instruction, which
    /// includes a bunch of information.
    CALL,

    /// Return with a flag operand. Operand 0 is the chain operand, operand
    /// 1 is the number of bytes of stack to pop.
    RET_FLAG,

    /// Z80 compare
    CMP,

    /// Z80 conditional branches. Operand 0 is the chain operand, operand 1
    /// is the block to branch if condition is true, operand 2 is the
    /// condition code, and operand 3 is the flag operand produced by a CP
    /// instruction.
    BR_CC
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

  bool useSoftFloat() const override { return true; }
  bool isSelectSupported(SelectSupportKind /*Kind*/) const override {
    return false;
  }
  bool canOpTrap(unsigned Op, EVT VT) const override { return false; }
  bool allowsMisalignedMemoryAccesses(EVT /*VT*/, unsigned /*AddrSpace*/,
                                      unsigned /*Align*/,
                                      bool *Fast) const override {
    if (Fast != nullptr)
      *Fast = true;
    return true;
  }
  

  /// This method returs the name of a target specific DAG node.
  const char *getTargetNodeName(unsigned Opcode) const override;

  /// Provide custom lowering hooks for some operations.
  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;
  SDValue LowerLibCall(RTLIB::Libcall LC24, RTLIB::Libcall LC16,
                       SDValue Op, SelectionDAG &DAG) const;
  SDValue NarrowOperation(SDValue Op, SelectionDAG &DAG) const;

  /// Return true if the addressing mode represented by AM is legal for this
  /// target, for a load/store of the specified type.
  bool isLegalAddressingMode(const DataLayout &DL, const AddrMode &AM,
                             Type *Ty, unsigned AS) const override;

  /// Return true if the specified immediate is a legal icmp immediate, that is
  /// the target has icmp instructions which can compare a register against the
  /// immediate without having to materialize the immediate into a register.
  bool isLegalICmpImmediate(int64_t Imm) const override;

  /// Return true if the specified immediate is a legal add immediate, that is
  /// the target has add instructions which can add a register and the immediate
  /// without having to materialize the immediate into a register.
  bool isLegalAddImmediate(int64_t Imm) const override;

  /// Return true if it's free to truncate a value of
  /// type Ty1 to type Ty2. e.g. On z80 it's free to truncate an i16 value in
  /// register HL to i8 by referencing its sub-register L.
  bool isTruncateFree(Type *Ty1, Type *Ty2) const override;
  bool isTruncateFree(EVT VT1, EVT VT2) const override;

  /// Return true if any actual instruction that defines a
  /// value of type Ty1 implicit zero-extends the value to Ty2 in the result
  /// register. This does not necessarily include registers defined in
  /// unknown ways, such as incoming arguments, or copies from unknown
  /// virtual registers. Also, if isTruncateFree(Ty2, Ty1) is true, this
  /// does not necessarily apply to truncate instructions. e.g. on ez80,
  /// all instructions that define 16-bit values implicit zero-extend the
  /// result out to 24 bits.
  bool isZExtFree(Type *Ty1, Type *Ty2) const override;
  bool isZExtFree(EVT VT1, EVT VT2) const override;

  /// Return true if it's profitable to narrow operations of type VT1 to
  /// VT2. e.g. on ez80, it's profitable to narrow from i24 to i8 but not from
  /// i24 to i16.
  bool isNarrowingProfitable(EVT VT1, EVT VT2) const override;

  /// \brief Returns true if it is beneficial to convert a load of a constant
  /// to just the constant itself.
  bool shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                         Type *Ty) const override;

  /// Replace the results of node with an illegal result type with new values
  /// buildt out of custom code.
  void ReplaceNodeResults(SDNode *N, SmallVectorImpl<SDValue> &Results,
                          SelectionDAG &DAG) const override;

  MachineBasicBlock *
    EmitInstrWithCustomInserter(MachineInstr *MI,
                                MachineBasicBlock *BB) const override;

private:
  SDValue EmitCMP(SDValue &LHS, SDValue &RHS, SDValue &TargetCC,
                  ISD::CondCode CC, const SDLoc &DL, SelectionDAG &DAG) const;

  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;

  CCAssignFn *getCCAssignFn(CallingConv::ID CallConv) const;

  SDValue LowerFormalArguments(SDValue Chain,
                               CallingConv::ID CallConv, bool isVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerCall(CallLoweringInfo &CLI,
                    SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerCallResult(SDValue Chain, SDValue InFlag,
                          CallingConv::ID CallConv, bool IsVarArg,
                          const SmallVectorImpl<ISD::InputArg> &Ins,
                          SDLoc DL, SelectionDAG &DAG,
                          SmallVectorImpl<SDValue> &InVals) const;
  SDValue LowerReturn(SDValue Chain,
                      CallingConv::ID CallConv, bool isVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutVals,
                      const SDLoc &DL, SelectionDAG &DAG) const override;

  EVT getTypeForExtReturn(LLVMContext &Context, EVT VT,
                          ISD::NodeType ExtendKind) const override;

  MachineBasicBlock *EmitLoweredCmp(MachineInstr *MI,
                                    MachineBasicBlock *BB) const;
};
} // End llvm namespace

#endif
