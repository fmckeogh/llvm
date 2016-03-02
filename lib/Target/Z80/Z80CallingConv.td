//===-- Z80CallingConv.td - Calling Conventions Z80/EZ80 ---*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the Z80 and EZ80 architectures.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Return Value Calling Conventions
//===----------------------------------------------------------------------===//
def RetCC_Z80_C : CallingConv<[
  CCIfType<[i1],  CCPromoteToType<i8>>,
  CCIfType<[i8],  CCAssignToReg<[A]>>,
  CCIfType<[i16], CCAssignToReg<[HL]>>,
  CCIfType<[i32], CCAssignToRegWithShadow<[HL], [DE]>>
]>;
def RetCC_EZ80_C : CallingConv<[
  CCIfType<[i1],  CCPromoteToType<i8>>,
  CCIfType<[i8],  CCAssignToReg<[A]>>,
  CCIfType<[i16], CCAssignToReg<[HL]>>,
  CCIfType<[i24], CCAssignToReg<[UHL]>>,
  CCIfType<[i32], CCAssignToRegWithShadow<[UHL], [E]>>
]>;

//===----------------------------------------------------------------------===//
// Z80 Argument Calling Conventions
//===----------------------------------------------------------------------===//

def CC_Z80_C : CallingConv<[
  CCIfByVal<CCPassByVal<2, 1>>,
  CCIfType<[i1, i8, i16], CCAssignToStack<2, 1>>,
  CCIfType<[i32, f32], CCAssignToStack<4, 1>>,
  CCIfType<[i64, f64], CCAssignToStack<8, 1>>
]>;
def CC_EZ80_C : CallingConv<[
  CCIfByVal<CCPassByVal<3, 1>>,
  CCIfType<[i1, i8, i16, i24], CCAssignToStack<3, 1>>,
  CCIfType<[i32, f32], CCAssignToStack<6, 1>>,
  CCIfType<[i64, f64], CCAssignToStack<9, 1>>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved Registers.
//===----------------------------------------------------------------------===//

def CSR_Z80_C : CalleeSavedRegs<(add IX)>;
def CSR_EZ80_C : CalleeSavedRegs<(add UIX)>;
