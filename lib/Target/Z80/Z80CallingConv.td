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
  CCIfType<[i8],  CCAssignToReg<[A, C, E, L]>>,
  CCIfType<[i16], CCAssignToReg<[HL, BC, DE, IY]>>,
  CCIfType<[i24], CCAssignToReg<[UHL, UBC, UDE, UIY]>>,
  CCIfType<[i32], CCAssignToReg<[EUHL, AUBC]>>
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
def CC_Z80_LC : CallingConv<[
  CCIfType<[i8],  CCAssignToReg<[A, C]>>,
  CCIfType<[i16], CCAssignToReg<[HL, BC]>>,
  CCIfType<[i24], CCAssignToReg<[UHL, UBC]>>,
  CCIfType<[i32], CCAssignToReg<[EUHL, AUBC]>>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved Registers.
//===----------------------------------------------------------------------===//

def CSR_NoRegs  : CalleeSavedRegs<(add)>;
def CSR_Z80_C   : CalleeSavedRegs<(add IX)>;
def CSR_EZ80_C  : CalleeSavedRegs<(add UIX)>;
def CSR_Z80_LC  : CalleeSavedRegs<(add A, L16)>;
def CSR_EZ80_LC : CalleeSavedRegs<(add A, L24)>;
