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
def RetCC_Z80_LC_L  : CallingConv<[
  CCIfType<[i24], CCAssignToReg<[UBC]>>,
  CCIfType<[i8], CCAssignToReg<[A]>>
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
  CCIfType<[i32, f32, i48], CCAssignToStack<6, 1>>,
  CCIfType<[i64, f64], CCAssignToStack<9, 1>>
]>;

def CC_Z80_LC : CallingConv<[
  CCIfType<[i24], CCIfSplit<CCAssignToReg<[UHL, UBC]>>>,
  CCIfType<[i24], CCAssignToRegWithShadow<[UHL, UDE, UBC], [E, L, A]>>,
  CCIfType<[i16], CCAssignToReg<[HL, DE, BC, IY]>>,
  CCIfType<[i1],  CCPromoteToType<i8>>,
  CCIfType<[i8], CCIfSplitEnd<CCAssignToReg<[E, A]>>>
]>;
def RetCC_Z80_C : CallingConv<[
  CCDelegateTo<CC_Z80_LC>,
  CCIfType<[i8], CCAssignToReg<[A, E]>>
]>;
def CC_Z80_LC_AB : CallingConv<[
  CCDelegateTo<CC_Z80_LC>,
  CCIfType<[i8], CCAssignToReg<[A, B]>>
]>;
def CC_Z80_LC_AC : CallingConv<[
  CCDelegateTo<CC_Z80_LC>,
  CCIfType<[i8], CCAssignToReg<[A, C]>>
]>;
def CC_Z80_LC_BC : CallingConv<[
  CCDelegateTo<CC_Z80_LC>,
  CCIfType<[i8], CCAssignToReg<[B, C]>>
]>;
def CC_Z80_LC_C  : CallingConv<[
  CCDelegateTo<CC_Z80_LC>,
  CCIfType<[i8], CCAssignToReg<[C]>>
]>;
def CC_Z80_LC_L  : CallingConv<[
  CCIfType<[i24], CCAssignToReg<[UBC]>>,
  CCIfType<[i8], CCAssignToReg<[A, L]>>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved Registers.
//===----------------------------------------------------------------------===//

def CSR_NoRegs  : CalleeSavedRegs<(add)>;
def CSR_Z80_C   : CalleeSavedRegs<(add IX)>;
def CSR_EZ80_C  : CalleeSavedRegs<(add UIX)>;
def CSR_Z80_LC  : CalleeSavedRegs<(add S16, SPS)>;
def CSR_EZ80_LC : CalleeSavedRegs<(add S24, SPL)>;
