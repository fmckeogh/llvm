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

class Z80Reg<string n, bits<16> Enc = -1> : Register<n> {
  let Namespace = "Z80";
  let HWEncoding = Enc;
}
class Z80RegWithSubRegs<string n, list<Register> sub = [], bits<16> enc = -1>
  : Z80Reg<n, enc> {
  let SubRegs = sub;
}

// Subregister indices.
let Namespace = "Z80" in {
  def sub_low   : SubRegIndex<8>;
  def sub_high  : SubRegIndex<8, 8>;
  def sub_short : SubRegIndex<16>;
  def sub_long  : SubRegIndex<24>;
  def sub_top   : SubRegIndex<8, 24>;
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
def IXL : Z80Reg<"ixl", 5>;
def IYH : Z80Reg<"iyh", 4>;
def IYL : Z80Reg<"iyl", 5>;
}

let SubRegIndices = [sub_high, sub_low], CoveredBySubRegs = 1 in {
// 16-bit registers
def AF : Z80RegWithSubRegs<"af", [A,F], 3>;
def BC : Z80RegWithSubRegs<"bc", [B,C], 0>;
def DE : Z80RegWithSubRegs<"de", [D,E], 1>;
def HL : Z80RegWithSubRegs<"hl", [H,L], 2>;
// 16-bit index registers
let CostPerUse = 1 in {
def IX : Z80RegWithSubRegs<"ix", [IXH,IXL], 2>;
def IY : Z80RegWithSubRegs<"iy", [IYH,IYL], 2>;
}
}
def SPS : Z80Reg<"sp", 3>;

let SubRegIndices = [sub_short] in {
// 24-bit registers
def UBC : Z80RegWithSubRegs<"bc", [BC], 0>;
def UDE : Z80RegWithSubRegs<"de", [DE], 1>;
def UHL : Z80RegWithSubRegs<"hl", [HL], 2>;
// 24-bit index registers
let CostPerUse = 1 in {
def UIX : Z80RegWithSubRegs<"ix", [IX], 2>;
def UIY : Z80RegWithSubRegs<"iy", [IY], 2>;
}
}
def SPL : Z80Reg<"sp", 3>;
def PC  : Z80Reg<"pc">;

let SubRegIndices = [sub_top, sub_long], CoveredBySubRegs = 1 in {
// 32-bit pseudo registers
// 
def   AUBC : Z80RegWithSubRegs<"", [  A,UBC]>;
def   AUDE : Z80RegWithSubRegs<"", [  A,UDE]>;
def   AUHL : Z80RegWithSubRegs<"", [  A,UHL]>;
def   AUIX : Z80RegWithSubRegs<"", [  A,UIX]>;
def   AUIY : Z80RegWithSubRegs<"", [  A,UIY]>;
def   ASPL : Z80RegWithSubRegs<"", [  A,SPL]>;
def   BUDE : Z80RegWithSubRegs<"", [  B,UDE]>;
def   BUHL : Z80RegWithSubRegs<"", [  B,UHL]>;
def   BUIX : Z80RegWithSubRegs<"", [  B,UIX]>;
def   BUIY : Z80RegWithSubRegs<"", [  B,UIY]>;
def   BSPL : Z80RegWithSubRegs<"", [  B,SPL]>;
def   CUDE : Z80RegWithSubRegs<"", [  C,UDE]>;
def   CUHL : Z80RegWithSubRegs<"", [  C,UHL]>;
def   CUIX : Z80RegWithSubRegs<"", [  C,UIX]>;
def   CUIY : Z80RegWithSubRegs<"", [  C,UIY]>;
def   CSPL : Z80RegWithSubRegs<"", [  C,SPL]>;
def   DUBC : Z80RegWithSubRegs<"", [  D,UBC]>;
def   DUHL : Z80RegWithSubRegs<"", [  D,UHL]>;
def   DUIX : Z80RegWithSubRegs<"", [  D,UIX]>;
def   DUIY : Z80RegWithSubRegs<"", [  D,UIY]>;
def   DSPL : Z80RegWithSubRegs<"", [  D,SPL]>;
def   EUBC : Z80RegWithSubRegs<"", [  E,UBC]>;
def   EUHL : Z80RegWithSubRegs<"", [  E,UHL]>;
def   EUIX : Z80RegWithSubRegs<"", [  E,UIX]>;
def   EUIY : Z80RegWithSubRegs<"", [  E,UIY]>;
def   ESPL : Z80RegWithSubRegs<"", [  E,SPL]>;
def   HUBC : Z80RegWithSubRegs<"", [  H,UBC]>;
def   HUDE : Z80RegWithSubRegs<"", [  H,UDE]>;
def   HUIX : Z80RegWithSubRegs<"", [  H,UIX]>;
def   HUIY : Z80RegWithSubRegs<"", [  H,UIY]>;
def   HSPL : Z80RegWithSubRegs<"", [  H,SPL]>;
def   LUBC : Z80RegWithSubRegs<"", [  L,UBC]>;
def   LUDE : Z80RegWithSubRegs<"", [  L,UDE]>;
def   LUIX : Z80RegWithSubRegs<"", [  L,UIX]>;
def   LUIY : Z80RegWithSubRegs<"", [  L,UIY]>;
def   LSPL : Z80RegWithSubRegs<"", [  L,SPL]>;
def IXHUBC : Z80RegWithSubRegs<"", [IXH,UBC]>;
def IXHUDE : Z80RegWithSubRegs<"", [IXH,UDE]>;
def IXHUHL : Z80RegWithSubRegs<"", [IXH,UHL]>;
def IXHUIY : Z80RegWithSubRegs<"", [IXH,UIY]>;
def IXHSPL : Z80RegWithSubRegs<"", [IXH,SPL]>;
def IXLUBC : Z80RegWithSubRegs<"", [IXL,UBC]>;
def IXLUDE : Z80RegWithSubRegs<"", [IXL,UDE]>;
def IXLUHL : Z80RegWithSubRegs<"", [IXL,UHL]>;
def IXLUIY : Z80RegWithSubRegs<"", [IXL,UIY]>;
def IXLSPL : Z80RegWithSubRegs<"", [IXL,SPL]>;
def IYHUBC : Z80RegWithSubRegs<"", [IYH,UBC]>;
def IYHUDE : Z80RegWithSubRegs<"", [IYH,UDE]>;
def IYHUHL : Z80RegWithSubRegs<"", [IYH,UHL]>;
def IYHUIX : Z80RegWithSubRegs<"", [IYH,UIX]>;
def IYHSPL : Z80RegWithSubRegs<"", [IYH,SPL]>;
def IYLUBC : Z80RegWithSubRegs<"", [IYL,UBC]>;
def IYLUDE : Z80RegWithSubRegs<"", [IYL,UDE]>;
def IYLUHL : Z80RegWithSubRegs<"", [IYL,UHL]>;
def IYLUIX : Z80RegWithSubRegs<"", [IYL,UIX]>;
def IYLSPL : Z80RegWithSubRegs<"", [IYL,SPL]>;
}

//===----------------------------------------------------------------------===//
//  Register Class Definitions...
//

def aptr_rc : PointerLikeRegClass<1>;
def iptr_rc : PointerLikeRegClass<2>;

class Z80RC8 <dag regList> : RegisterClass<"Z80", [i8 ], 8, regList>;
class Z80RC16<dag regList> : RegisterClass<"Z80", [i16], 8, regList>;
class Z80RC24<dag regList> : RegisterClass<"Z80", [i24], 8, regList>;
class Z80RC32<dag regList> : RegisterClass<"Z80", [i32], 8, regList>;

def C8  : Z80RC8 <(add F)>;
def G8  : Z80RC8 <(add A, L, E, C, H, D, B)>;
def O8  : Z80RC8 <(add A, E, C, D, B)>;
def X8  : Z80RC8 <(add O8, IXL, IXH)>;
def Y8  : Z80RC8 <(add O8, IYL, IYH)>;
def I8  : Z80RC8 <(add IXL, IYL, IXH, IYH)>;
def R8  : Z80RC8 <(add G8, I8)>;

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

def A32 : Z80RC32<(add AUHL, AUIX, AUIY)>;
def R32 : Z80RC32<(add AUHL, CUHL, BUHL, EUHL, DUHL,
                       AUDE, CUDE, BUDE, LUDE, HUDE,
                       AUBC, EUBC, DUBC, LUBC, HUBC,
                       AUIY, CUIY, BUIY, EUIY, DUIY, LUIY, HUIY,
                       AUIX, CUIX, BUIX, EUIX, DUIX, LUIX, HUIX,
                       IYLUHL, IYHUHL, IXLUHL, IXHUHL,
                       IYLUDE, IYHUDE, IXLUDE, IXHUDE,
                       IYLUBC, IYHUBC, IXLUBC, IXHUBC,
                       IYLUIX, IYHUIX, IXLUIY, IXHUIY,
                       ASPL, CSPL, BSPL, ESPL, DSPL, LSPL, HSPL,
                       IYLSPL, IYHSPL, IXLSPL, IXHSPL)>;
