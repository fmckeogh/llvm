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
def NoPre   : Prefix<0>;
def CBPre   : Prefix<1>;
def DDPre   : Prefix<2>;
def DDCBPre : Prefix<3>;
def EDPre   : Prefix<4>;
def FDPre   : Prefix<5>;
def FDCBPre : Prefix<6>;

class Z80Inst<Prefix prfx, bits<8> opcod, dag outs, dag ins, list<dag> pattern,
              string asm = "", string con = "">
  : Instruction {
  let Namespace = "Z80";

  bits<8> Opcode = opcod;

  let OutOperandList = outs;
  let InOperandList = ins;
  let Pattern = pattern;
  let AsmString = asm;
  let Constraints = con;

  let isCodeGenOnly = 1;
}

class BI<Prefix p, bits<8> o, string op, string args, string con,
         dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<p, o, outs, ins, pattern, !strconcat(op,        "\t", args), con>;
class SI<Prefix p, bits<8> o, string op, string args, string con,
         dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<p, o, outs, ins, pattern, !strconcat(op, "{|.sis}\t", args), con>;
class LI<Prefix p, bits<8> o, string op, string args, string con,
         dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<p, o, outs, ins, pattern, !strconcat(op, "{.lil|}\t", args), con>,
    Requires<[HaveEZ80Ops]>;

class P<dag outs = (outs), dag ins = (ins), list<dag> pattern = []>
  : Z80Inst<?, ?, outs, ins, pattern> {
  let isPseudo = 1;
}
class I<bits<8> o, dag outs = (outs), dag ins = (ins), list<dag> pattern = []>
  : Z80Inst<NoPre, o, outs, ins, pattern>;
class PI<Prefix prfx, bits<8> o, dag outs, dag ins, list<dag> pattern = []>
  : Z80Inst<prfx, o, outs, ins, pattern>;
