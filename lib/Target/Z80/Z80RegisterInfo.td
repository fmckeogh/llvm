//===- Z80RegisterInfo.td - Describe the Z80 Register File --*- tablegen -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the Z80 Register file, defining the registers themselves,
// aliases between the registers, and the register classes built out of the
// registers.
//
//===----------------------------------------------------------------------===//

class Z80Reg<string n, bits<16> Enc = -1, list<Register> subregs = []>
  : Register<n> {
  let Namespace = "Z80";
  let HWEncoding = Enc;
  let SubRegs = subregs;
}

// Subregister indices.
let Namespace = "X86" in {
  def sub_low   : SubRegIndex<8>;
  def sub_high  : SubRegIndex<8, 8>;
  def sub_short : SubRegIndex<16>;
}

//===----------------------------------------------------------------------===//
//  Register definitions...
//

// 8-bit registers
def A : Z80Reg<"a", 7>;
def F : Z80Reg<"f">;
def B : Z80Reg<"b", 0>;
def C : Z80Reg<"c", 1>;
def D : Z80Reg<"d", 2>;
def E : Z80Reg<"e", 3>;
def H : Z80Reg<"h", 4>;
def L : Z80Reg<"l", 5>;

// 8-bit index registers
let CostPerUse = 1 in {
def IXH : Z80Reg<"ixh", 4>;
def IXL : Z80Reg<"ixh", 5>;
def IYH : Z80Reg<"iyl", 4>;
def IYL : Z80Reg<"iyl", 5>;
}

let SubRegIndices = [sub_high, sub_low], CoveredBySubRegs = 1 in {
// 16-bit registers
def AF : Z80Reg<"af", 3, [A,F]>;
def BC : Z80Reg<"bc", 0, [B,C]>;
def DE : Z80Reg<"de", 1, [D,E]>;
def HL : Z80Reg<"hl", 2, [H,L]>;
// 16-bit index registers
let CostPerUse = 1 in {
def IX : Z80Reg<"ix", 2, [IXH,IXL]>;
def IY : Z80Reg<"iy", 2, [IYH,IYL]>;
}
}
def SPS : Z80Reg<"sp", 3>;

let SubRegIndices = [sub_short] in {
// 24-bit registers
def UBC : Z80Reg<"bc", 0, [BC]>;
def UDE : Z80Reg<"de", 1, [DE]>;
def UHL : Z80Reg<"hl", 2, [HL]>;
// 24-bit index registers
let CostPerUse = 1 in {
def UIX : Z80Reg<"ix", 2, [IX]>;
def UIY : Z80Reg<"iy", 2, [IY]>;
}
}
def SPL : Z80Reg<"sp", 3>;
def PC  : Z80Reg<"pc">;

//===----------------------------------------------------------------------===//
//  Register Class Definitions...
//

class Z80RC8 <dag regList> : RegisterClass<"Z80", [i8],  8, regList>;
class Z80RC16<dag regList> : RegisterClass<"Z80", [i16], 8, regList>;
class Z80RC24<dag regList> : RegisterClass<"Z80", [i24], 8, regList>;

def  G8 :  Z80RC8<(add A, L, E, C, H, D, B)>;
def  O8 :  Z80RC8<(add A, E, C, D, B)>;
def  X8 :  Z80RC8<(add O8, IXL, IXH)>;
def  Y8 :  Z80RC8<(add O8, IYL, IYH)>;
def  I8 :  Z80RC8<(add IXL, IYL, IXH, IYL)>;
def  R8 :  Z80RC8<(add G8, I8)>;
def  A8 :  Z80RC8<(add A)>;

def O16 : Z80RC16<(add DE, BC, SPS)>;
def G16 : Z80RC16<(add HL, O16)>;
def X16 : Z80RC16<(add IX, O16)>;
def Y16 : Z80RC16<(add IY, O16)>;
def I16 : Z80RC16<(add IX, IY)>;
def R16 : Z80RC16<(add G16, I16)>;
def L16 : Z80RC16<(sub R16, SPS)>;
def S16 : Z80RC16<(add L16, AF)>;
def A16 : Z80RC16<(add HL, I16)>;

def O24 : Z80RC24<(add UDE, UBC, SPL)>;
def G24 : Z80RC24<(add UHL, O24)>;
def X24 : Z80RC24<(add UIX, O24)>;
def Y24 : Z80RC24<(add UIY, O24)>;
def I24 : Z80RC24<(add UIX, UIY)>;
def R24 : Z80RC24<(add G24, I24)>;
def L24 : Z80RC24<(sub R24, SPL)>;
def S24 : Z80RC24<(add L24, AF)>;
def A24 : Z80RC24<(add UHL, I24)>;
