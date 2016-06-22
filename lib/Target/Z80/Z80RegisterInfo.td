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

class Z80Reg<string n, bits<16> Enc = -1>
  : Register<n> {
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
def   AUBC : Z80RegWithSubRegs<  "abc", [  A,UBC]>;
def   AUDE : Z80RegWithSubRegs<  "ade", [  A,UDE]>;
def   AUHL : Z80RegWithSubRegs<  "ahl", [  A,UHL]>;
def   AUIX : Z80RegWithSubRegs<  "aix", [  A,UIX]>;
def   AUIY : Z80RegWithSubRegs<  "aiy", [  A,UIY]>;
def   ASPL : Z80RegWithSubRegs<  "asp", [  A,SPL]>;
def   BUDE : Z80RegWithSubRegs<  "bde", [  B,UDE]>;
def   BUHL : Z80RegWithSubRegs<  "bhl", [  B,UHL]>;
def   BUIX : Z80RegWithSubRegs<  "bix", [  B,UIX]>;
def   BUIY : Z80RegWithSubRegs<  "biy", [  B,UIY]>;
def   BSPL : Z80RegWithSubRegs<  "bsp", [  B,SPL]>;
def   CUDE : Z80RegWithSubRegs<  "cde", [  C,UDE]>;
def   CUHL : Z80RegWithSubRegs<  "chl", [  C,UHL]>;
def   CUIX : Z80RegWithSubRegs<  "cix", [  C,UIX]>;
def   CUIY : Z80RegWithSubRegs<  "ciy", [  C,UIY]>;
def   CSPL : Z80RegWithSubRegs<  "csp", [  C,SPL]>;
def   DUBC : Z80RegWithSubRegs<  "dbc", [  D,UBC]>;
def   DUHL : Z80RegWithSubRegs<  "dhl", [  D,UHL]>;
def   DUIX : Z80RegWithSubRegs<  "dix", [  D,UIX]>;
def   DUIY : Z80RegWithSubRegs<  "diy", [  D,UIY]>;
def   DSPL : Z80RegWithSubRegs<  "dsp", [  D,SPL]>;
def   EUBC : Z80RegWithSubRegs<  "ebc", [  E,UBC]>;
def   EUHL : Z80RegWithSubRegs<  "ehl", [  E,UHL]>;
def   EUIX : Z80RegWithSubRegs<  "eix", [  E,UIX]>;
def   EUIY : Z80RegWithSubRegs<  "eiy", [  E,UIY]>;
def   ESPL : Z80RegWithSubRegs<  "esp", [  E,SPL]>;
def   HUBC : Z80RegWithSubRegs<  "hbc", [  H,UBC]>;
def   HUDE : Z80RegWithSubRegs<  "hde", [  H,UDE]>;
def   HUIX : Z80RegWithSubRegs<  "hix", [  H,UIX]>;
def   HUIY : Z80RegWithSubRegs<  "hiy", [  H,UIY]>;
def   HSPL : Z80RegWithSubRegs<  "hsp", [  H,SPL]>;
def   LUBC : Z80RegWithSubRegs<  "lbc", [  L,UBC]>;
def   LUDE : Z80RegWithSubRegs<  "lde", [  L,UDE]>;
def   LUIX : Z80RegWithSubRegs<  "lix", [  L,UIX]>;
def   LUIY : Z80RegWithSubRegs<  "liy", [  L,UIY]>;
def   LSPL : Z80RegWithSubRegs<  "lsp", [  L,SPL]>;
def IXHUBC : Z80RegWithSubRegs<"ixhbc", [IXH,UBC]>;
def IXHUDE : Z80RegWithSubRegs<"ixhde", [IXH,UDE]>;
def IXHUHL : Z80RegWithSubRegs<"ixhhl", [IXH,UHL]>;
def IXHUIY : Z80RegWithSubRegs<"ixhiy", [IXH,UIY]>;
def IXHSPL : Z80RegWithSubRegs<"ixhsp", [IXH,SPL]>;
def IXLUBC : Z80RegWithSubRegs<"ixlbc", [IXL,UBC]>;
def IXLUDE : Z80RegWithSubRegs<"ixlde", [IXL,UDE]>;
def IXLUHL : Z80RegWithSubRegs<"ixlhl", [IXL,UHL]>;
def IXLUIY : Z80RegWithSubRegs<"ixliy", [IXL,UIY]>;
def IXLSPL : Z80RegWithSubRegs<"ixlsp", [IXL,SPL]>;
def IYHUBC : Z80RegWithSubRegs<"iyhbc", [IYH,UBC]>;
def IYHUDE : Z80RegWithSubRegs<"iyhde", [IYH,UDE]>;
def IYHUHL : Z80RegWithSubRegs<"iyhhl", [IYH,UHL]>;
def IYHUIX : Z80RegWithSubRegs<"iyhix", [IYH,UIX]>;
def IYHSPL : Z80RegWithSubRegs<"iyhsp", [IYH,SPL]>;
def IYLUBC : Z80RegWithSubRegs<"iylbc", [IYL,UBC]>;
def IYLUDE : Z80RegWithSubRegs<"iylde", [IYL,UDE]>;
def IYLUHL : Z80RegWithSubRegs<"iylhl", [IYL,UHL]>;
def IYLUIX : Z80RegWithSubRegs<"iylix", [IYL,UIX]>;
def IYLSPL : Z80RegWithSubRegs<"iylsp", [IYL,SPL]>;
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
def R32 : Z80RC32<(add AUBC, AUDE, AUHL, AUIX, AUIY, ASPL,
                       BUDE, BUHL, BUIX, BUIY, BSPL,
                       CUDE, CUHL, CUIX, CUIY, CSPL,
                       DUBC, DUHL, DUIX, DUIY, DSPL,
                       EUBC, EUHL, EUIX, EUIY, ESPL,
                       HUBC, HUDE, HUIX, HUIY, HSPL,
                       LUBC, LUDE, LUIX, LUIY, LSPL,
                       IXHUBC, IXHUDE, IXHUHL, IXHUIY, IXHSPL,
                       IXLUBC, IXLUDE, IXLUHL, IXLUIY, IXLSPL,
                       IYHUBC, IYHUDE, IYHUHL, IYHUIX, IYHSPL,
                       IYLUBC, IYLUDE, IYLUHL, IYLUIX, IYLSPL)>;
