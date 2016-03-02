//===-- Z80InstrFormats.td - Z80 Instruction Formats -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Z80 Instruction Format Definitions.
//

class Prefix<bits<3> val> {
  bits<3> Value = val;
}
def NoPrfx   : Prefix<0>;
def ExtPrfx  : Prefix<1>;
def BSPrfx   : Prefix<2>;
def IXPrfx   : Prefix<3>;
def IXBSPrfx : Prefix<4>;
def IYPrfx   : Prefix<5>;
def IYXSPrfx : Prefix<6>;

class Z80Inst<Prefix prfx, bits<8> opcod, dag outs, dag ins, list<dag> pattern>
  : Instruction {
  let Namespace = "Z80";

  bits<8> Opcode = opcod;

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let Pattern = pattern;
}

class I<bits<8> o, dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<NoPrfx, o, outs, ins, pattern>;
class PI<Prefix prfx, bits<8> o, dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<prfx, o, outs, ins, pattern>;
