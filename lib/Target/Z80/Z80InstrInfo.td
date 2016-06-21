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
// Type Constraints.
//===----------------------------------------------------------------------===//
class SDTCisI8 <int OpNum> : SDTCisVT<OpNum, i8>;
class SDTCisI16<int OpNum> : SDTCisVT<OpNum, i16>;
class SDTCisI24<int OpNum> : SDTCisVT<OpNum, i24>;
class SDTCisPtr<int OpNum> : SDTCisVT<OpNum, iPTR>;

//===----------------------------------------------------------------------===//
// Type Profiles.
//===----------------------------------------------------------------------===//
def SDT_Z80mlt           : SDTypeProfile<1, 1, [SDTCisI16<0>, SDTCisI16<1>]>;
def SDT_Z80Call          : SDTypeProfile<0, -1, [SDTCisPtr<0>]>;
def SDT_Z80CallSeqStart  : SDCallSeqStart<[SDTCisI16<0>]>;
def SDT_Z80CallSeqEnd    : SDCallSeqEnd<[SDTCisI16<0>, SDTCisI16<1>]>;
def SDT_Z80BrCC          : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>,
                                                SDTCisVT<1, i8>]>;

//===----------------------------------------------------------------------===//
// Z80 specific DAG Nodes.
//

def Z80mlt           : SDNode<"Z80ISD::MLT", SDT_Z80mlt>;
def Z80retflag       : SDNode<"Z80ISD::RET_FLAG", SDTNone,
                              [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Z80call          : SDNode<"Z80ISD::CALL", SDT_Z80Call,
                              [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                               SDNPVariadic]>;
def Z80callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_Z80CallSeqStart,
                              [SDNPHasChain, SDNPOutGlue]>;
def Z80callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_Z80CallSeqEnd,
                              [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def Z80brcc          : SDNode<"Z80ISD::BRCC", SDT_Z80BrCC,
                              [SDNPHasChain, SDNPInGlue]>;

//===----------------------------------------------------------------------===//
// Z80 Instruction Format Definitions.
//

include "Z80InstrFormats.td"

//===----------------------------------------------------------------------===//
// Pattern fragments.
//

// Z80 specific condition code. These correspond to CondCode in
// Z80InstrInfo.h. They must be kept in synch.
def Z80_COND_NZ : PatLeaf<(i8 0)>;
def Z80_COND_Z  : PatLeaf<(i8 1)>;
def Z80_COND_NC : PatLeaf<(i8 2)>;
def Z80_COND_C  : PatLeaf<(i8 3)>;
def Z80_COND_PO : PatLeaf<(i8 4)>;
def Z80_COND_PE : PatLeaf<(i8 5)>;
def Z80_COND_P  : PatLeaf<(i8 6)>;
def Z80_COND_M  : PatLeaf<(i8 7)>;

//===----------------------------------------------------------------------===//
// Z80 Operand Definitions.
//

def imem : Operand<iPTR> {
  let PrintMethod = "printImmMem";
  let MIOperandInfo = (ops imm);
  let OperandType = "OPERAND_MEMORY";
}

def rmem16 : Operand<i16> {
  let PrintMethod = "printRegOffMem";
  let MIOperandInfo = (ops A16, i8imm);
}
def rmem24 : Operand<i24> {
  let PrintMethod = "printRegOffMem";
  let MIOperandInfo = (ops A24, i8imm);
}

def rmem : Operand<iPTR> {
  let PrintMethod = "printRegOffMem";
  let MIOperandInfo = (ops aptr_rc, i8imm);
  let OperandType = "OPERAND_MEMORY";
}

def i24imm : Operand<i24> { let OperandType = "OPERAND_IMMEDIATE"; }

def jmptarget : Operand<OtherVT>;
def jmptargetoff : Operand<OtherVT>;

def cc : Operand<i8> {
  let PrintMethod = "printCCOperand";
}

//===----------------------------------------------------------------------===//
// Pattern Fragments.
//
def imm_long_XFORM : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getZExtValue() & 0xffffff,
                                   SDLoc(N), MVT::i24);
}]>;
def imm_top_XFORM : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getZExtValue() >> 24, SDLoc(N), MVT::i8);
}]>;

//===----------------------------------------------------------------------===//
// Z80 Complex Pattern Definitions.
//
def iaddr : ComplexPattern<iPTR, 1, "SelectImmMem",
                           [tglobaladdr, texternalsym]>;
def raddr : ComplexPattern<iPTR, 2, "SelectRegOffMem",
                           [add, sub, frameindex]>;

//===----------------------------------------------------------------------===//
// Z80 Instruction Predicate Definitions.
//
def In16BitMode : Predicate<"Subtarget->is16Bit()">,
                  AssemblerPredicate<"Mode16Bit", "16-bit mode">;
def In24BitMode : Predicate<"Subtarget->is24Bit()">,
                  AssemblerPredicate<"Mode24Bit", "24-bit mode">;

//===----------------------------------------------------------------------===//
// Instruction list.
//

let Defs = [SPS, HL, F], Uses = [SPS] in {
def ADJCALLSTACKDOWN16 : P<(outs), (ins i16imm:$amt),
                           [(Z80callseq_start timm:$amt)]>,
                         Requires<[In16BitMode]>;
def ADJCALLSTACKUP16   : P<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                           [(Z80callseq_end timm:$amt1, timm:$amt2)]>,
                         Requires<[In16BitMode]>;
}
let Defs = [SPL, UHL, F], Uses = [SPL] in {
def ADJCALLSTACKDOWN24 : P<(outs), (ins i16imm:$amt),
                           [(Z80callseq_start timm:$amt)]>,
                         Requires<[In24BitMode]>;
def ADJCALLSTACKUP24   : P<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                           [(Z80callseq_end timm:$amt1, timm:$amt2)]>,
                         Requires<[In24BitMode]>;
}

let AsmString = "nop" in def NOP : I<0x00, (outs), (ins), []>;

//===----------------------------------------------------------------------===//
//  Control Flow Instructions.
//

// All calls clobber the non-callee saved registers.  SP is marked as a use to
// prevent stack-pointer assignments that appear immediately before calls from
// potentially appearing dead.  Uses for argument registers are added manually.
let AsmString = "call\t$dst", isCall = 1 in {
  let Uses = [SPS] in
  def CALL16im : I<0xCD, (outs), (ins i16imm:$dst), [(Z80call timm:$dst)]>;
  let Uses = [SPL] in
  def CALL24im : I<0xCD, (outs), (ins i24imm:$dst), [(Z80call timm:$dst)]>;
}

let AsmString = "ret", isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  def RET : I<0xC9, (outs), (ins), [(Z80retflag)]>;
}

let isBranch = 1, isTerminator = 1, isBarrier = 1 in {
  let AsmString = "jq\t$dst" in
  def JQ : I<?, (outs), (ins jmptarget:$dst), [(br bb:$dst)]>;
  let AsmString = "jr\t$dst" in
  def JR : I<0x18, (outs), (ins jmptargetoff:$dst)>;
  let AsmString = "jp\t$dst" in
  def JP : I<0xC3, (outs), (ins jmptarget:$dst)>;
}
let isBranch = 1, isTerminator = 1, Uses = [F] in {
  let AsmString = "jq\t$cc, $dst" in
  def JQCC : I<?, (outs), (ins jmptarget:$dst, cc:$cc), [(Z80brcc bb:$dst, imm:$cc)]>;
  let AsmString = "jr\t$cc, $dst" in
  def JRCC : I<0x18, (outs), (ins jmptargetoff:$dst, cc:$cc)>;
  let AsmString = "jp\t$cc, $dst" in
  def JPCC : I<0xC3, (outs), (ins jmptarget:$dst, cc:$cc)>;
}

//===----------------------------------------------------------------------===//
//  Load Instructions.
//

let AsmString = "ld\t$dst, $src" in {
def  LD8rr :  I<        0x40, (outs R8:$dst), (ins R8:$src)>;
def  LD8xx : PI<IXPrfx, 0x40, (outs X8:$dst), (ins X8:$src)>;
def  LD8yy : PI<IYPrfx, 0x40, (outs Y8:$dst), (ins Y8:$src)>;

def  LD8ri : I<0x06, (outs  R8:$dst), (ins  i8imm:$src),
               [(set  R8:$dst, imm:$src)]>;
def LD16ri : I<0x01, (outs R16:$dst), (ins i16imm:$src),
               [(set R16:$dst, imm:$src)]>;
def LD24ri : I<0x01, (outs R24:$dst), (ins i24imm:$src),
               [(set R24:$dst, imm:$src)]>;

def LD16rim : I<0x2A, (outs R16:$dst), (ins imem:$src),
               [(set R16:$dst, (i16 (load iaddr:$src)))]>;
def LD24rim : I<0x2A, (outs R24:$dst), (ins imem:$src),
               [(set R24:$dst, (i24 (load iaddr:$src)))]>;

def  LD8rrm : I<0x46, (outs  R8:$dst), (ins rmem:$src),
               [(set  R8:$dst, (i8  (load raddr:$src)))]>;
def LD16rrm : I<0x46, (outs R16:$dst), (ins rmem:$src),
               [(set R16:$dst, (i16 (load raddr:$src)))]>;
def LD24rrm : I<0x46, (outs R24:$dst), (ins rmem:$src),
               [(set R24:$dst, (i24 (load raddr:$src)))]>;

def LD16imr : I<0x22, (outs), (ins imem:$dst, R16:$src),
               [(store R16:$src, iaddr:$dst)]>;
def LD24imr : I<0x22, (outs), (ins imem:$dst, R24:$src),
               [(store R24:$src, iaddr:$dst)]>;

def  LD8rmr : I<0x32, (outs), (ins rmem:$dst,  R8:$src),
               [(store  R8:$src, raddr:$dst)]>;
def LD16rmr : I<0x22, (outs), (ins rmem:$dst, R16:$src),
               [(store R16:$src, raddr:$dst)]>;
def LD24rmr : I<0x22, (outs), (ins rmem:$dst, R24:$src),
               [(store R24:$src, raddr:$dst)]>;
}

let AsmString = "ld\ta, $src", Defs = [A] in
def  LD8rim : I<0x3A, (outs), (ins imem:$src),
               [(set  A, (i8  (load iaddr:$src)))]>;
let AsmString = "ld\t$dst, a", Uses = [A] in
def  LD8imr : I<0x32, (outs), (ins imem:$dst),
               [(store  A, iaddr:$dst)]>;

let AsmString = "ld\tsp, $reg" in {
let Defs = [SPS] in
def LD16SP : I<0xF9, (outs), (ins A16:$reg)>;
let Defs = [SPL] in
def LD24SP : I<0xF9, (outs), (ins A24:$reg)>;
}

let Defs = [DE, HL], Uses = [DE, HL] in
def EX16DE : SI<NoPrfx, 0xEB, "ex", "de, hl", "", (outs), (ins)>;
let Defs = [UDE, UHL], Uses = [UDE, UHL] in
def EX24DE : LI<NoPrfx, 0xEB, "ex", "de, hl", "", (outs), (ins)>;

let AsmString = "pop\t$reg" in {
def  POP16r : I<0xC1, (outs S16:$reg), (ins)>;
def  POP24r : I<0xC1, (outs S24:$reg), (ins)>;
}

let AsmString = "push\t$reg" in {
def PUSH16r : I<0xC5, (outs), (ins S16:$reg)>;
def PUSH24r : I<0xC5, (outs), (ins S24:$reg)>;
}


let Defs = [F] in {
def ADD16ao : SI<NoPrfx, 0x09, "add", "$dst, $src", "$imp = $dst",
                 (outs A16:$dst), (ins A16:$imp, O16:$src),
                 [(set A16:$dst, (add A16:$imp, O16:$src)), (implicit F)]>;
def ADD24ao : LI<NoPrfx, 0x09, "add", "$dst, $src", "$imp = $dst",
                 (outs A24:$dst), (ins A24:$imp, O24:$src),
                 [(set A24:$dst, (add A24:$imp, O24:$src)), (implicit F)]>;
def ADD16aa : SI<NoPrfx, 0x29, "add", "$dst, $src", "$src = $dst",
                 (outs A16:$dst), (ins A16:$src),
                 [(set A16:$dst, (add A16:$src, A16:$src)), (implicit F)]>;
def ADD24aa : LI<NoPrfx, 0x29, "add", "$dst, $src", "$src = $dst",
                 (outs A24:$dst), (ins A24:$src),
                 [(set A24:$dst, (add A24:$src, A24:$src)), (implicit F)]>;
let Uses = [F] in {
def SBC16ao : SI<ExtPrfx, 0x42, "sbc", "$dst, $src", "$imp = $dst",
                 (outs A16:$dst), (ins A16:$imp, O16:$src),
                 [(set A16:$dst, (sube A16:$imp, O16:$src)), (implicit F)]>;
def SBC24ao : LI<ExtPrfx, 0x42, "sbc", "$dst, $src", "$imp = $dst",
                 (outs A24:$dst), (ins A24:$imp, O24:$src),
                 [(set A24:$dst, (sube A24:$imp, O24:$src)), (implicit F)]>;
def SBC16aa : SI<ExtPrfx, 0x62, "sbc", "$dst, $src", "$imp = $dst, $src = $dst",
                 (outs A16:$dst), (ins A16:$imp, A16:$src),
                 [(set A16:$dst, (sube A16:$imp, A16:$src)), (implicit F)]>;
def SBC24aa : LI<ExtPrfx, 0x62, "sbc", "$dst, $src", "$imp = $dst, $src = $dst",
                 (outs A24:$dst), (ins A24:$imp, A24:$src),
                 [(set A24:$dst, (sube A24:$imp, A24:$src)), (implicit F)]>;
def ADC16ao : SI<ExtPrfx, 0x4A, "adc", "$dst, $src", "$imp = $dst",
                 (outs A16:$dst), (ins A16:$imp, O16:$src),
                 [(set A16:$dst, (adde A16:$imp, O16:$src)), (implicit F)]>;
def ADC24ao : LI<ExtPrfx, 0x4A, "adc", "$dst, $src", "$imp = $dst",
                 (outs A24:$dst), (ins A24:$imp, O24:$src),
                 [(set A24:$dst, (adde A24:$imp, O24:$src)), (implicit F)]>;
def ADC16aa : SI<ExtPrfx, 0x6A, "adc", "$dst, $src", "$imp = $dst, $src = $dst",
                 (outs A16:$dst), (ins A16:$imp, A16:$src),
                 [(set A16:$dst, (adde A16:$imp, A16:$src)), (implicit F)]>;
def ADC24aa : LI<ExtPrfx, 0x6A, "adc", "$dst, $src", "$imp = $dst, $src = $dst",
                 (outs A24:$dst), (ins A24:$imp, A24:$src),
                 [(set A24:$dst, (adde A24:$imp, A24:$src)), (implicit F)]>;
}
}

let Defs = [F] in {
def INC8r  : BI<NoPrfx, 0x04, "inc", "$dst", "$dst = $imp",
                (outs R8 :$dst), (ins R8 :$imp),
                [(set R8 :$dst, (add R8 :$imp, 1)), (implicit F)]>;
def DEC8r  : BI<NoPrfx, 0x05, "dec", "$dst", "$dst = $imp",
                (outs R8: $dst), (ins R8 :$imp),
                [(set R8 :$dst, (sub R8 :$imp, 1)), (implicit F)]>;
}
def INC16r : SI<NoPrfx, 0x03, "inc", "$dst", "$dst = $imp",
                (outs R16:$dst), (ins R16:$imp),
                [(set R16:$dst, (add R16:$imp, 1))]>;
def DEC16r : SI<NoPrfx, 0x0B, "dec", "$dst", "$dst = $imp",
                (outs R16:$dst), (ins R16:$imp),
                [(set R16:$dst, (sub R16:$imp, 1))]>;
def INC24r : LI<NoPrfx, 0x03, "inc", "$dst", "$dst = $imp",
                (outs R24:$dst), (ins R24:$imp),
                [(set R24:$dst, (add R24:$imp, 1))]>;
def DEC24r : LI<NoPrfx, 0x0B, "dec", "$dst", "$dst = $imp",
                (outs R24:$dst), (ins R24:$imp),
                [(set R24:$dst, (sub R24:$imp, 1))]>;

let Defs = [A, F], Uses = [A] in {
let AsmString = "add\ta, $arg" in {
def ADD8rr  : I<0x80, (outs), (ins R8:$arg),
                [(set A, (add A, R8:$arg)), (implicit F)]>;
def ADD8ri  : I<0xC6, (outs), (ins i8imm:$arg),
                [(set A, (add A, imm:$arg)), (implicit F)]>;
def ADD8rrm : I<0x86, (outs), (ins rmem:$arg),
                [(set A, (add A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
let AsmString = "sub\ta, $arg" in {
def SUB8rr  : I<0x90, (outs), (ins R8:$arg),
                [(set A, (sub A, R8:$arg)), (implicit F)]>;
def SUB8ri  : I<0xD6, (outs), (ins i8imm:$arg),
                [(set A, (sub A, imm:$arg)), (implicit F)]>;
def SUB8rrm : I<0x96, (outs), (ins rmem:$arg),
                [(set A, (sub A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
let Uses = [A, F] in {
let AsmString = "adc\ta, $arg" in {
def ADC8rr  : I<0x87, (outs), (ins R8:$arg),
                [(set A, (adde A, R8:$arg)), (implicit F)]>;
def ADC8ri  : I<0xCE, (outs), (ins i8imm:$arg),
                [(set A, (adde A, imm:$arg)), (implicit F)]>;
def ADC8rrm : I<0x8E, (outs), (ins rmem:$arg),
                [(set A, (adde A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
}
let Uses = [A, F] in {
let AsmString = "sbc\ta, $arg" in {
def SBC8rr  : I<0x97, (outs), (ins R8:$arg),
                [(set A, (sube A, R8:$arg)), (implicit F)]>;
def SBC8ri  : I<0xDE, (outs), (ins i8imm:$arg),
                [(set A, (sube A, imm:$arg)), (implicit F)]>;
def SBC8rrm : I<0x9E, (outs), (ins rmem:$arg),
                [(set A, (sube A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
}
let AsmString = "and\ta, $arg" in {
def AND8rr  : I<0xA0, (outs), (ins R8:$arg),
                [(set A, (and A, R8:$arg)), (implicit F)]>;
def AND8ri  : I<0xE6, (outs), (ins i8imm:$arg),
                [(set A, (and A, imm:$arg)), (implicit F)]>;
def AND8rrm : I<0xA6, (outs), (ins rmem:$arg),
                [(set A, (and A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
let AsmString = "xor\ta, $arg" in {
def XOR8rr  : I<0xA8, (outs), (ins R8:$arg),
                [(set A, (xor A, R8:$arg)), (implicit F)]>;
def XOR8ri  : I<0xEE, (outs), (ins i8imm:$arg),
                [(set A, (xor A, imm:$arg)), (implicit F)]>;
def XOR8rrm : I<0xAE, (outs), (ins rmem:$arg),
                [(set A, (xor A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
let AsmString =  "or\ta, $arg" in {
def  OR8rr  : I<0xB0, (outs), (ins R8:$arg),
                [(set A, (or  A, R8:$arg)), (implicit F)]>;
def  OR8ri  : I<0xF6, (outs), (ins i8imm:$arg),
                [(set A, (or  A, imm:$arg)), (implicit F)]>;
def  OR8rrm : I<0xB6, (outs), (ins rmem:$arg),
                [(set A, (or  A, (i8 (load raddr:$arg)))), (implicit F)]>;
}
}

def LEA16rr : SI<ExtPrfx, 0x02, "lea", "$dst, $src", "",
                 (outs L16:$dst), (ins rmem16:$src),
                 [(set L16:$dst, raddr:$src)]>;
def LEA24rr : LI<ExtPrfx, 0x02, "lea", "$dst, $src", "",
                 (outs L24:$dst), (ins rmem24:$src),
                 [(set L24:$dst, raddr:$src)]>;

let AsmString = "mlt\t$dst", Constraints = "$src = $dst" in {
def MLT8rr : PI<ExtPrfx, 0x4C, (outs G16:$dst), (ins G16:$src),
                [(set G16:$dst, (Z80mlt G16:$src))]>;
}

//===----------------------------------------------------------------------===//
// Non-Instruction Patterns.
//===----------------------------------------------------------------------===//

// calls
def : Pat<(Z80call (i16 tglobaladdr :$dst)), (CALL16im tglobaladdr :$dst)>;
def : Pat<(Z80call (i16 texternalsym:$dst)), (CALL16im texternalsym:$dst)>;
def : Pat<(Z80call (i24 tglobaladdr :$dst)), (CALL24im tglobaladdr :$dst)>;
def : Pat<(Z80call (i24 texternalsym:$dst)), (CALL24im texternalsym:$dst)>;

//===----------------------------------------------------------------------===//
// Subsystems.
//===----------------------------------------------------------------------===//

// mlt
//def : Pat<(i8 (mul R8:$arg1, R8:$arg2)),
//          (SUBREG_TO_REG (i24 0), (MLT8rr (INSERT_SUBREG R8:$arg1)), sub_low)>;

def : Pat<(i32 (imm:$src)),
          (REG_SEQUENCE R32, (LD24ri (imm_long_XFORM imm:$src)), sub_long,
                             (LD8ri (imm_top_XFORM imm:$src)), sub_top)>;

// anyexts
def : Pat<(i16 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i24 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i24 (anyext R16:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R16:$src, sub_short)>;
def : Pat<(i32 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i32 (anyext R16:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R16:$src, sub_short)>;
def : Pat<(i32 (anyext R24:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R24:$src, sub_long)>;

//   zexts
def : Pat<(i16 (  zext R8 :$src)),
          (REG_SEQUENCE R16, R8 :$src, sub_low, (LD8ri 0), sub_high)>;
def : Pat<(i24 (  zext R8 :$src)),
          (INSERT_SUBREG (LD24ri 0), R8 :$src, sub_low)>;
def : Pat<(i24 (  zext R16:$src)),
          (INSERT_SUBREG (LD24ri 0), R16:$src, sub_short)>;
def : Pat<(i32 (  zext R8 :$src)),
          (REG_SEQUENCE R32, (LD8ri (i8 0)), sub_top,
                        (INSERT_SUBREG (LD24ri (i24 0)), R8 :$src, sub_low),
                        sub_long/*, (LD8ri (i8 0)), sub_top*/)>;
def : Pat<(i32 (  zext R16:$src)),
          (REG_SEQUENCE R32, (LD8ri (i8 0)), sub_top,
                        (INSERT_SUBREG (LD24ri (i24 0)), R16:$src, sub_short),
                        sub_long/*, (LD8ri (i8 0)), sub_top*/)>;
/*def : Pat<(i24 (  zext R8 :$src)),
          (INSERT_SUBREG (i24 (SBC24aa (i24 (IMPLICIT_DEF)),
                                       (i24 (IMPLICIT_DEF)),
                                       (i8 (OR8rr A, A)))),
                         R8 :$src, sub_low)>;*/

// truncs
def : Pat<(i8  (trunc R16:$src)), (EXTRACT_SUBREG R16:$src, sub_low)>;
def : Pat<(i8  (trunc R24:$src)), (EXTRACT_SUBREG R24:$src, sub_low)>;
def : Pat<(i16 (trunc R24:$src)), (EXTRACT_SUBREG R24:$src, sub_short)>;

/*
// Any instruction that defines a 16-bit result leaves the high half of the
// register zero. Truncate can be lowered to EXTRACT_SUBREG. CopyFromReg may
// be copying from a truncate. But any other 16-bit operation will zero-extend
// up to 24 bits.
def def16 : PatLeaf<(i16 R16:$src), [{
  dbgs() << "Hello World!\n";
  return N->getOpcode() != ISD::TRUNCATE &&
         N->getOpcode() != TargetOpcode::EXTRACT_SUBREG &&
         N->getOpcode() != ISD::CopyFromReg;
}]>;

// In the case of a 16-bit def that is known to implicitly zero-extend,
// we can use a SUBREG_TO_REG.
def : Pat<(i24 (zext def16:$src)),
          (SUBREG_TO_REG (i24 0), R16:$src, sub_short)>;
*/

// Compiler Pseudo Instructions and Pat Patterns
//include "Z80InstrCompiler.td"
