//===-- Z80InstrInfo.td - Main Z80 Instruction Definition --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the Z80 instruction set, defining the instructions, and
// properties of the instructions which are needed for code generation, machine
// code emission, and analysis.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Z80 specific DAG Nodes.
//

def Z80retflag : SDNode<"Z80ISD::RET_FLAG", SDTNone,
                        [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//===----------------------------------------------------------------------===//
// Z80 Instruction Format Definitions.
//

include "Z80InstrFormats.td"

//===----------------------------------------------------------------------===//
// Z80 Operand Definitions.
//

def imem : Operand<iPTR> {
  let PrintMethod = "printImmMem";
  let MIOperandInfo = (ops imm);
  let OperandType = "OPERAND_MEMORY";
}

def raddr16 : Operand<i16> {
  let PrintMethod = "printRegOffAddr";
  let MIOperandInfo = (ops I16, i8imm);
}
def raddr24 : Operand<i24> {
  let PrintMethod = "printRegOffAddr";
  let MIOperandInfo = (ops I24, i8imm);
}

def rmem : Operand<iPTR> {
  let PrintMethod = "printRegOffMem";
  let MIOperandInfo = (ops ptr_rc, i8imm);
  let OperandType = "OPERAND_MEMORY";
}

def i24imm : Operand<i24> { let OperandType = "OPERAND_IMMEDIATE"; }

//===----------------------------------------------------------------------===//
// Pattern Fragments.
//


//===----------------------------------------------------------------------===//
// Z80 Complex Pattern Definitions.
//
def imempat : ComplexPattern<iPTR, 1, "SelectImmMem">;
def rmempat : ComplexPattern<iPTR, 2, "SelectRegOffMem">;

//===----------------------------------------------------------------------===//
// Instruction list.
//

let AsmString = "nop" in def NOP : I<0x00, (outs), (ins), []>;

//===----------------------------------------------------------------------===//
//  Control Flow Instructions.
//

let AsmString = "ret", isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  def RET : I<0xC9, (outs), (ins), [(Z80retflag)]>;
}

//===----------------------------------------------------------------------===//
//  Load Instructions.
//

let AsmString = "ld\t$dst, $src" in {
def LD8rr  :  I<        0x40, (outs R8:$dst), (ins R8:$src)>;
def LD8rrx : PI<IXPrfx, 0x40, (outs X8:$dst), (ins X8:$src)>;
def LD8rry : PI<IYPrfx, 0x40, (outs Y8:$dst), (ins Y8:$src)>;

def  LD8ri : I<0x06, (outs  R8:$dst), (ins  i8imm:$src),
               [(set  R8:$dst, imm:$src)]>;
def LD16ri : I<0x01, (outs R16:$dst), (ins i16imm:$src),
               [(set R16:$dst, imm:$src)]>;
def LD24ri : I<0x01, (outs R24:$dst), (ins i24imm:$src),
               [(set R24:$dst, imm:$src)]>;

def LD16rim : I<0x2A, (outs R16:$dst), (ins imem:$src),
               [(set R16:$dst, (i16 (load imempat:$src)))]>;
def LD24rim : I<0x2A, (outs R24:$dst), (ins imem:$src),
               [(set R24:$dst, (i24 (load imempat:$src)))]>;

def  LD8rrm : I<0x46, (outs  R8:$dst), (ins rmem:$src),
               [(set  R8:$dst, (i8  (load rmempat:$src)))]>;
def LD16rrm : I<0x46, (outs R16:$dst), (ins rmem:$src),
               [(set R16:$dst, (i16 (load rmempat:$src)))]>;
def LD24rrm : I<0x46, (outs R24:$dst), (ins rmem:$src),
               [(set R24:$dst, (i24 (load rmempat:$src)))]>;

def LD16imr : I<0x22, (outs), (ins imem:$dst, R16:$src),
               [(store R16:$src, imempat:$dst)]>;
def LD24imr : I<0x22, (outs), (ins imem:$dst, R24:$src),
               [(store R24:$src, imempat:$dst)]>;

def  LD8rmr : I<0x32, (outs), (ins rmem:$dst,  R8:$src),
               [(store  R8:$src, rmempat:$dst)]>;
def LD16rmr : I<0x22, (outs), (ins rmem:$dst, R16:$src),
               [(store R16:$src, rmempat:$dst)]>;
def LD24rmr : I<0x22, (outs), (ins rmem:$dst, R24:$src),
               [(store R24:$src, rmempat:$dst)]>;
}

let AsmString = "ld\ta, $src", Defs = [A] in
def  LD8rim : I<0x3A, (outs), (ins imem:$src),
               [(set  A, (i8  (load imempat:$src)))]>;
let AsmString = "ld\t$dst, a", Uses = [A] in
def  LD8imr : I<0x32, (outs), (ins imem:$dst),
               [(store  A, imempat:$dst)]>;

let AsmString = "ld\tsp, $reg" in {
let Defs = [SPS] in
def LD16sp : I<0xF9, (outs), (ins A16:$reg)>;
let Defs = [SPL] in
def LD24sp : I<0xF9, (outs), (ins A24:$reg)>;
}

let AsmString = "pop\t$reg" in {
def  POP16r : I<0xC1, (outs S16:$reg), (ins)>;
def  POP24r : I<0xC1, (outs S24:$reg), (ins)>;
}

let AsmString = "push\t$reg" in {
def PUSH16r : I<0xC5, (outs), (ins S16:$reg)>;
def PUSH24r : I<0xC5, (outs), (ins S24:$reg)>;
}

let AsmString = "add\thl, $reg" in {
let Defs = [HL] in
def ADD16rr : I<0x09, (outs), (ins G16:$reg)>;
let Defs = [UHL] in
def ADD24rr : I<0x09, (outs), (ins G24:$reg)>;
}
let AsmString = "add\tix, $reg" in {
let Defs = [IX] in
def ADD16rrx : PI<IXPrfx, 0x09, (outs), (ins X16:$reg)>;
let Defs = [UIX] in
def ADD24rrx : PI<IXPrfx, 0x09, (outs), (ins X24:$reg)>;
}
let AsmString = "add\tiy, $reg" in {
let Defs = [IY] in
def ADD16rry : PI<IYPrfx, 0x09, (outs), (ins Y16:$reg)>;
let Defs = [UIY] in
def ADD24rry : PI<IYPrfx, 0x09, (outs), (ins Y24:$reg)>;
}

let AsmString = "lea\t$dst, $src" in {
def LEA16rr : PI<ExtPrfx, 0x02, (outs L16:$dst), (ins raddr16:$src)>;
def LEA24rr : PI<ExtPrfx, 0x02, (outs L24:$dst), (ins raddr24:$src)>;
}

let Defs = [F] in {
let AsmString = "inc\t$reg" in {
def INC8r  : I<0x04, (outs  R8:$reg), (ins  R8:$src),
               [(set  R8:$reg, (add  R8:$src, 1))]>;
def INC16r : I<0x03, (outs R16:$reg), (ins R16:$src),
               [(set R16:$reg, (add R16:$src, 1))]>;
def INC24r : I<0x03, (outs R24:$reg), (ins R24:$src),
               [(set R24:$reg, (add R24:$src, 1))]>;
}

let AsmString = "dec\t$reg" in {
def DEC8r  : I<0x05, (outs  R8:$reg), (ins  R8:$src),
               [(set  R8:$reg, (sub  R8:$src, 1))]>;
def DEC16r : I<0x0B, (outs R16:$reg), (ins R16:$src),
               [(set R16:$reg, (sub R16:$src, 1))]>;
def DEC24r : I<0x0B, (outs R24:$reg), (ins R24:$src),
               [(set R24:$reg, (sub R24:$src, 1))]>;
}
}
