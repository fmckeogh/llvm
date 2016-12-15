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
class SDTCisI8  <int OpNum> : SDTCisVT<OpNum, i8>;
class SDTCisFlag<int OpNum> : SDTCisI8<OpNum>;
class SDTCisI16 <int OpNum> : SDTCisVT<OpNum, i16>;
class SDTCisI24 <int OpNum> : SDTCisVT<OpNum, i24>;
class SDTCisPtr <int OpNum> : SDTCisVT<OpNum, iPTR>;

//===----------------------------------------------------------------------===//
// Type Profiles.
//===----------------------------------------------------------------------===//
def SDTUnOpRF   : SDTypeProfile<2, 1, [SDTCisInt<0>,
                                       SDTCisFlag<1>,
                                       SDTCisSameAs<2, 0>]>;
def SDTUnOpRFF  : SDTypeProfile<2, 2, [SDTCisInt<0>,
                                       SDTCisFlag<1>,
                                       SDTCisSameAs<2, 0>,
                                       SDTCisFlag<3>]>;
def SDTBinOpRF  : SDTypeProfile<2, 2, [SDTCisInt<0>,
                                       SDTCisFlag<1>,
                                       SDTCisSameAs<2, 0>,
                                       SDTCisSameAs<3, 0>]>;
def SDTBinOpRFF : SDTypeProfile<2, 3, [SDTCisInt<0>,
                                       SDTCisFlag<1>,
                                       SDTCisSameAs<2, 0>,
                                       SDTCisSameAs<3, 0>,
                                       SDTCisFlag<4>]>;
def SDTBinOpF   : SDTypeProfile<1, 2, [SDTCisFlag<0>,
                                       SDTCisInt<1>,
                                       SDTCisSameAs<2, 1>]>;

def SDTZ80Wrapper        : SDTypeProfile<1, 1, [SDTCisPtrTy<0>,
                                                SDTCisSameAs<1, 0>]>;
def SDT_Z80mlt           : SDTypeProfile<1, 1, [SDTCisI16<0>, SDTCisI16<1>]>;
def SDT_Z80TCRet         : SDTypeProfile<0, 1, [SDTCisPtrTy<0>]>;
def SDT_Z80Call          : SDTypeProfile<0, -1, [SDTCisPtr<0>]>;
def SDT_Z80CallSeqStart  : SDCallSeqStart<[SDTCisPtr<0>]>;
def SDT_Z80CallSeqEnd    : SDCallSeqEnd<[SDTCisPtr<0>, SDTCisPtr<1>]>;
def SDT_Z80BrCond        : SDTypeProfile<0, 3, [SDTCisVT<0, OtherVT>,
                                                SDTCisI8<1>,
                                                SDTCisI8<2>]>;
def SDT_Z80Select        : SDTypeProfile<1, 4, [SDTCisInt<0>,
                                                SDTCisSameAs<1, 0>,
                                                SDTCisSameAs<2, 0>,
                                                SDTCisI8<3>,
                                                SDTCisI8<4>]>;

//===----------------------------------------------------------------------===//
// Z80 specific DAG Nodes.
//

def Z80Wrapper       : SDNode<"Z80ISD::Wrapper", SDTZ80Wrapper>;
def Z80rlc_flag      : SDNode<"Z80ISD::RLC",     SDTUnOpRF>;
def Z80rrc_flag      : SDNode<"Z80ISD::RRC",     SDTUnOpRF>;
def Z80rl_flag       : SDNode<"Z80ISD::RL",      SDTUnOpRFF>;
def Z80rr_flag       : SDNode<"Z80ISD::RR",      SDTUnOpRFF>;
def Z80sla_flag      : SDNode<"Z80ISD::SLA",     SDTUnOpRF>;
def Z80sra_flag      : SDNode<"Z80ISD::SRA",     SDTUnOpRF>;
def Z80srl_flag      : SDNode<"Z80ISD::SRL",     SDTUnOpRF>;
def Z80inc_flag      : SDNode<"Z80ISD::INC",     SDTUnOpRF>;
def Z80dec_flag      : SDNode<"Z80ISD::DEC",     SDTUnOpRF>;
def Z80add_flag      : SDNode<"Z80ISD::ADD",     SDTBinOpRF, [SDNPCommutative]>;
def Z80adc_flag      : SDNode<"Z80ISD::ADC",     SDTBinOpRFF>;
def Z80sub_flag      : SDNode<"Z80ISD::SUB",     SDTBinOpRF>;
def Z80sbc_flag      : SDNode<"Z80ISD::SBC",     SDTBinOpRFF>;
def Z80and_flag      : SDNode<"Z80ISD::AND",     SDTBinOpRF, [SDNPCommutative]>;
def Z80xor_flag      : SDNode<"Z80ISD::XOR",     SDTBinOpRF, [SDNPCommutative]>;
def Z80or_flag       : SDNode<"Z80ISD::OR",      SDTBinOpRF, [SDNPCommutative]>;
def Z80cp_flag       : SDNode<"Z80ISD::CP",      SDTBinOpF>;
def Z80tst_flag      : SDNode<"Z80ISD::TST",     SDTBinOpF,  [SDNPCommutative]>;
def Z80mlt           : SDNode<"Z80ISD::MLT",     SDT_Z80mlt>;
def Z80retflag       : SDNode<"Z80ISD::RET_FLAG", SDTNone,
                              [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Z80tcret         : SDNode<"Z80ISD::TC_RETURN", SDT_Z80TCRet,
                              [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Z80call          : SDNode<"Z80ISD::CALL", SDT_Z80Call,
                              [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                               SDNPVariadic]>;
def Z80callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_Z80CallSeqStart,
                              [SDNPHasChain, SDNPOutGlue]>;
def Z80callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_Z80CallSeqEnd,
                              [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def Z80brcond        : SDNode<"Z80ISD::BRCOND", SDT_Z80BrCond, [SDNPHasChain]>;
def Z80select        : SDNode<"Z80ISD::SELECT", SDT_Z80Select>;

//===----------------------------------------------------------------------===//
// Z80 Instruction Predicate Definitions.
//
def In16BitMode  : Predicate<"Subtarget->is16Bit()">,
                   AssemblerPredicate<"Mode16Bit", "16-bit mode">;
def In24BitMode  : Predicate<"Subtarget->is24Bit()">,
                   AssemblerPredicate<"Mode24Bit", "24-bit mode">;
def HaveUndocOps : Predicate<"Subtarget->hasUndocOps()">,
                   AssemblerPredicate<"FeatureUndoc", "undocumented ops">;
def HaveZ180Ops  : Predicate<"Subtarget->hasZ180Ops()">,
                   AssemblerPredicate<"FeatureZ180", "Z180 ops">;
def HaveEZ80Ops  : Predicate<"Subtarget->hasEZ80Ops()">,
                   AssemblerPredicate<"FeatureEZ80", "eZ80 ops">;
def HaveIdxHalf  : Predicate<"Subtarget->hasIndexHalfRegs()">,
                   AssemblerPredicate<"FeatureIdxHalf", "index half regs">;

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

def mem : Operand<iPTR> {
  let PrintMethod = "printMem";
  let MIOperandInfo = (ops imm);
  let OperandType = "OPERAND_MEMORY";
}
def ptr : Operand<iPTR> {
  let PrintMethod = "printPtr";
  let MIOperandInfo = (ops aptr_rc);
  let OperandType = "OPERAND_MEMORY";
}
def off : Operand<iPTR> {
  let PrintMethod = "printOff";
  let MIOperandInfo = (ops iptr_rc, i8imm);
  let OperandType = "OPERAND_MEMORY";
}
def off16 : Operand<i16> {
  let PrintMethod = "printAddr";
  let MIOperandInfo = (ops I16, i8imm);
}
def off24 : Operand<i24> {
  let PrintMethod = "printAddr";
  let MIOperandInfo = (ops I24, i8imm);
}

let OperandType = "OPERAND_IMMEDIATE" in def i24imm : Operand<i24>;

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
def mempat : ComplexPattern<iPTR, 1, "SelectMem",
                            [imm, globaladdr, externalsym]>;
def offpat : ComplexPattern<iPTR, 2, "SelectOff",
                            [add, sub, frameindex]>;

//===----------------------------------------------------------------------===//
// Instruction list.
//

let hasPostISelHook = 1 in {
  let Defs = [SPS, F], Uses = [SPS] in {
  def ADJCALLSTACKDOWN16 : P<(outs), (ins i16imm:$amt),
                             [(Z80callseq_start timm:$amt)]>,
                           Requires<[In16BitMode]>;
  def ADJCALLSTACKUP16   : P<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                             [(Z80callseq_end timm:$amt1, timm:$amt2)]>,
                           Requires<[In16BitMode]>;
  }
  let Defs = [SPL, F], Uses = [SPL] in {
  def ADJCALLSTACKDOWN24 : P<(outs), (ins i24imm:$amt),
                             [(Z80callseq_start timm:$amt)]>,
                           Requires<[In24BitMode]>;
  def ADJCALLSTACKUP24   : P<(outs), (ins i24imm:$amt1, i24imm:$amt2),
                             [(Z80callseq_end timm:$amt1, timm:$amt2)]>,
                           Requires<[In24BitMode]>;
  }
}
let usesCustomInserter = 1 in {
  def Select8  : P<(outs  R8:$dst), (ins  R8:$true,  R8:$false, i8imm:$cc),
                   [(set  R8:$dst, (Z80select  R8:$true,  R8:$false, imm:$cc,
                                    F))]>;
  def Select16 : P<(outs R16:$dst), (ins R16:$true, R16:$false, i8imm:$cc),
                   [(set R16:$dst, (Z80select R16:$true, R16:$false, imm:$cc,
                                    F))]>;
  def Select24 : P<(outs R24:$dst), (ins R24:$true, R24:$false, i8imm:$cc),
                   [(set R24:$dst, (Z80select R24:$true, R24:$false, imm:$cc,
                                    F))]>,
                   Requires<[In24BitMode]>;
  let Defs = [HL, F], Uses = [HL] in {
    def Sub016 : P<(outs), (ins), [(set  HL, F, (Z80sub_flag  HL, 0))]>;
    def Sub16  : P<(outs), (ins G16:$src),
                   [(set  HL, F, (Z80sub_flag  HL, G16:$src))]>;
  }
  let Defs = [UHL, F], Uses = [UHL] in {
    def Sub024 : P<(outs), (ins), [(set UHL, F, (Z80sub_flag UHL, 0))]>,
                 Requires<[In24BitMode]>;
    def Sub24 :  P<(outs), (ins G24:$src),
                   [(set UHL, F, (Z80sub_flag UHL, G24:$src))]>,
                 Requires<[In24BitMode]>;
  }
  let Defs = [F] in {
    let Uses = [HL] in {
      def Cp016 : P<(outs), (ins), [(set F, (Z80cp_flag HL, 0))]>;
      def Cp16  : P<(outs), (ins G16:$src),
                   [(set F, (Z80cp_flag HL, G16:$src))]>;
    }
    let Uses = [UHL] in {
      def Cp024 : P<(outs), (ins), [(set F, (Z80cp_flag UHL, 0))]>,
                  Requires<[In24BitMode]>;
      def Cp24  : P<(outs), (ins G24:$src),
                    [(set F, (Z80cp_flag UHL, G24:$src))]>,
                  Requires<[In24BitMode]>;
    }
  }
}
def : Pat<(sub   HL, G16:$src), (Sub16 G16:$src)>;
def : Pat<(subc  HL, G16:$src), (Sub16 G16:$src)>;
def : Pat<(sub  UHL, G24:$src), (Sub24 G24:$src)>;
def : Pat<(subc UHL, G24:$src), (Sub24 G24:$src)>;

let AsmString = "nop", hasSideEffects = 0 in
def NOP : I<0x00, (outs), (ins), []>;

//===----------------------------------------------------------------------===//
//  Control Flow Instructions.
//

// All calls clobber the non-callee saved registers.  SP is marked as a use to
// prevent stack-pointer assignments that appear immediately before calls from
// potentially appearing dead.  Uses for argument registers are added manually.
let AsmString = "call\t$dst", isCall = 1 in {
  let Uses = [SPS] in
  def CALL16i : I<0xCD, (outs), (ins i16imm:$dst), [(Z80call mempat:$dst)]>,
                Requires<[In16BitMode]>;
  let Uses = [SPL] in
  def CALL24i : I<0xCD, (outs), (ins i24imm:$dst), [(Z80call mempat:$dst)]>,
                Requires<[In24BitMode]>;
}
let isCall = 1 in {
  let Uses = [SPS] in
  def CALL16r : P<(outs), (ins A16:$dst), [(Z80call A16:$dst)]>,
                Requires<[In16BitMode]>;
  let Uses = [SPL] in
  def CALL24r : P<(outs), (ins A24:$dst), [(Z80call A24:$dst)]>,
                Requires<[In24BitMode]>;
}

let AsmString = "ret", isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  def RET : I<0xC9, (outs), (ins), [(Z80retflag)]>;
}
let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  let Uses = [SPS] in {
    def TCRETURN16i : P<(outs), (ins i16imm:$dst), [(Z80tcret mempat:$dst)]>,
                      Requires<[In16BitMode]>;
    def TCRETURN16r : P<(outs), (ins A16:$dst), [(Z80tcret A16:$dst)]>,
                      Requires<[In16BitMode]>;
  }
  let Uses = [SPL] in {
    def TCRETURN24i : P<(outs), (ins i24imm:$dst), [(Z80tcret mempat:$dst)]>,
                      Requires<[In24BitMode]>;
    def TCRETURN24r : P<(outs), (ins A24:$dst), [(Z80tcret A24:$dst)]>,
                      Requires<[In24BitMode]>;
  }
}

let isBranch = 1, isTerminator = 1, isBarrier = 1 in {
  let AsmString = "jq\t$target" in
  def JQ : I<?, (outs), (ins jmptarget:$target), [(br bb:$target)]>;
  let AsmString = "jr\t$target" in
  def JR : I<0x18, (outs), (ins jmptargetoff:$target)>;
  let AsmString = "jp\t$target" in {
    def JP : I<0xC3, (outs), (ins jmptarget:$target)>;
    let isIndirectBranch = 1 in
    def JPr : I<0xE9, (outs), (ins ptr:$target), [(brind iPTR:$target)]>;
  }
}
let isBranch = 1, isTerminator = 1, Uses = [F] in {
  let AsmString = "jq\t$cc, $target" in
  def JQCC : I<?, (outs), (ins jmptarget:$target, cc:$cc),
               [(Z80brcond bb:$target, imm:$cc, F)]>;
  let AsmString = "jr\t$cc, $target" in
  def JRCC : I<0x18, (outs), (ins jmptargetoff:$target, cc:$cc)>;
  let AsmString = "jp\t$cc, $target" in
  def JPCC : I<0xC3, (outs), (ins jmptarget:$target, cc:$cc)>;
}

//===----------------------------------------------------------------------===//
//  Load Instructions.
//

let AsmString = "ld\t$dst, $src" in {
def  LD8rr :  I<       0x40, (outs  R8:$dst), (ins  R8:$src)>;
def  LD8xx : PI<DDPre, 0x40, (outs  X8:$dst), (ins  X8:$src)>,
             Requires<[HaveIdxHalf]>;
def  LD8yy : PI<FDPre, 0x40, (outs  Y8:$dst), (ins  Y8:$src)>,
             Requires<[HaveIdxHalf]>;

let mayLoad = 1, canFoldAsLoad = 1 in {
def LD16rm : I<0x2A, (outs R16:$dst), (ins mem:$src),
               [(set R16:$dst, (i16 (load mempat:$src)))]>;
def LD24rm : I<0x2A, (outs R24:$dst), (ins mem:$src),
               [(set R24:$dst, (i24 (load mempat:$src)))]>,
             Requires<[HaveEZ80Ops]>;

def  LD8rp : I<0x46, (outs  R8:$dst), (ins ptr:$src),
               [(set  R8:$dst, (load iPTR:$src))]>;
def LD16rp : I<0x46, (outs R16:$dst), (ins ptr:$src),
               [(set R16:$dst, (load iPTR:$src))]>,
             Requires<[HaveEZ80Ops]>;
def LD88rp : P<(outs R16:$dst), (ins ptr:$src),
               [(set R16:$dst, (load iPTR:$src))]>;
def LD24rp : I<0x46, (outs R24:$dst), (ins ptr:$src),
               [(set R24:$dst, (load iPTR:$src))]>,
             Requires<[HaveEZ80Ops]>;

def  LD8ro : I<0x46, (outs  R8:$dst), (ins off:$src),
               [(set  R8:$dst, (load offpat:$src))]>;
def LD16ro : I<0x46, (outs R16:$dst), (ins off:$src),
               [(set R16:$dst, (load offpat:$src))]>,
             Requires<[HaveEZ80Ops]>;
def LD88ro : P<(outs R16:$dst), (ins off:$src),
               [(set R16:$dst, (load offpat:$src))]>;
def LD24ro : I<0x46, (outs R24:$dst), (ins off:$src),
               [(set R24:$dst, (load offpat:$src))]>,
             Requires<[HaveEZ80Ops]>;
}

let mayStore = 1 in {
def LD16mr : I<0x22, (outs), (ins mem:$dst, R16:$src),
               [(store R16:$src, mempat:$dst)]>;
def LD24mr : I<0x22, (outs), (ins mem:$dst, R24:$src),
               [(store R24:$src, mempat:$dst)]>,
             Requires<[HaveEZ80Ops]>;

def  LD8pr : I<0x32, (outs), (ins ptr:$dst,  R8:$src),
               [(store  R8:$src, iPTR:$dst)]>;
def LD16pr : I<0x22, (outs), (ins ptr:$dst, R16:$src),
               [(store R16:$src, iPTR:$dst)]>,
             Requires<[HaveEZ80Ops]>;
def LD88pr : P<(outs), (ins ptr:$dst, R16:$src),
               [(store R16:$src, iPTR:$dst)]>;
def LD24pr : I<0x22, (outs), (ins ptr:$dst, R24:$src),
               [(store R24:$src, iPTR:$dst)]>,
             Requires<[HaveEZ80Ops]>;

def  LD8or : I<0x32, (outs), (ins off:$dst,  R8:$src),
               [(store  R8:$src, offpat:$dst)]>;
def LD16or : I<0x22, (outs), (ins off:$dst, R16:$src),
               [(store R16:$src, offpat:$dst)]>,
             Requires<[HaveEZ80Ops]>;
def LD88or : P<(outs), (ins off:$dst, R16:$src),
               [(store R16:$src, offpat:$dst)]>;
def LD24or : I<0x22, (outs), (ins off:$dst, R24:$src),
               [(store R24:$src, offpat:$dst)]>,
             Requires<[HaveEZ80Ops]>;

def  LD8pi : I<0x36, (outs), (ins ptr:$dst,  i8imm:$src),
               [(store (i8 imm:$src), iPTR:$dst)]>;
def  LD8oi : I<0x36, (outs), (ins off:$dst,  i8imm:$src),
               [(store (i8 imm:$src), offpat:$dst)]>;
}

let isMoveImm = 1, isReMaterializable = 1 in {
def  LD8ri :  I<        0x06, (outs  R8:$dst), (ins  i8imm:$src),
                [(set  R8:$dst,    imm:$src)]>;
}
def LD16ri : SI<NoPre, 0x01, "ld", "$dst, $src", "",
                (outs R16:$dst), (ins i16imm:$src),
                [(set R16:$dst, mempat:$src)]>;
def LD24ri : LI<NoPre, 0x01, "ld", "$dst, $src", "",
                (outs R24:$dst), (ins i24imm:$src),
                [(set R24:$dst, mempat:$src)]>;
}

let AsmString = "ld\ta, $src", Defs = [A], mayLoad = 1 in
def  LD8am : I<0x3A, (outs), (ins mem:$src),
               [(set  A, (load mempat:$src))]>;
let AsmString = "ld\t$dst, a", Uses = [A], mayStore = 1 in
def  LD8ma : I<0x32, (outs), (ins mem:$dst),
               [(store  A, mempat:$dst)]>;

let AsmString = "ld\tsp, $src" in {
let Defs = [SPS] in
def LD16SP : SI<NoPre, 0xF9, "ld", "sp, $src", "", (outs), (ins A16:$src)>;
let Defs = [SPL] in
def LD24SP : LI<NoPre, 0xF9, "ld", "sp, $src", "", (outs), (ins A24:$src)>;
}

let Defs = [DE, HL], Uses = [DE, HL] in
def EX16DE : SI<NoPre, 0xEB, "ex", "de, hl", "", (outs), (ins)>;
let Defs = [UDE, UHL], Uses = [UDE, UHL] in
def EX24DE : LI<NoPre, 0xEB, "ex", "de, hl", "", (outs), (ins)>;

let Defs = [HL], Uses = [HL, SPS] in
def EX16SP : SI<NoPre, 0xE3, "ex", "(sp), hl", "", (outs), (ins)>;
let Defs = [UHL], Uses = [UHL, SPL] in
def EX24SP : LI<NoPre, 0xE3, "ex", "(sp), hl", "", (outs), (ins)>;

let AsmString = "pop\t$dst" in {
let Uses = [SPS] in
def  POP16r : I<0xC1, (outs S16:$dst), (ins)>;
let Uses = [SPL] in
def  POP24r : I<0xC1, (outs S24:$dst), (ins)>;
}

let AsmString = "push\t$src" in {
let Uses = [SPS] in
def PUSH16r : I<0xC5, (outs), (ins S16:$src)>;
let Uses = [SPL] in
def PUSH24r : I<0xC5, (outs), (ins S24:$src)>;
}

let Defs = [F], isReMaterializable = 1 in {
  def RCF : P;
  let AsmString = "scf" in
  def SCF : I<0x37>;
  let AsmString = "ccf", Uses = [F] in
  def CCF : I<0x37>;
}

//===----------------------------------------------------------------------===//
//  Arithmetic Instructions.
//

multiclass UnOp8RF<Prefix prefix, bits<8> opcode, string mnemonic> {
  let AsmString = !strconcat(mnemonic, "\t$arg"), Defs = [F] in {
    let Constraints = "$imp = $arg" in
    def 8r : PI<prefix, opcode, (outs R8:$imp), (ins R8:$arg),
                [(set R8:$imp, F,
                      (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                          R8:$arg))]>;
    def 8m : PI<prefix, opcode, (outs), (ins ptr:$arg),
                [(store (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                            (i8 (load iPTR:$arg))), iPTR:$arg),
                 (implicit F)]>;
    def 8o : PI<prefix, opcode, (outs), (ins off:$arg),
                [(store (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                            (i8 (load offpat:$arg))), offpat:$arg),
                 (implicit F)]>;
  }
}
multiclass UnOp8RFF<Prefix prefix, bits<8> opcode, string mnemonic> {
  let AsmString = !strconcat(mnemonic, "\t$arg"), Defs = [F], Uses = [F] in {
    let Constraints = "$imp = $arg" in
    def 8r : PI<prefix, opcode, (outs R8:$imp), (ins R8:$arg),
                [(set R8:$imp, F,
                      (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                          R8:$arg, F))]>;
    def 8m : PI<prefix, opcode, (outs), (ins ptr:$arg),
                [(store (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                            (i8 (load iPTR:$arg)), F), iPTR:$arg),
                 (implicit F)]>;
    def 8o : PI<prefix, opcode, (outs), (ins off:$arg),
                [(store (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                            (i8 (load offpat:$arg)), F), offpat:$arg),
                 (implicit F)]>;
  }
}
multiclass BinOp8RF<Prefix prefix, bits<3> opcode, string mnemonic,
                    bit compare = 0> {
  let AsmString = !strconcat(mnemonic, "\ta, $arg"),
      isCompare = compare, Defs = [A, F], Uses = [A] in {
    def 8ar : PI<prefix, {0b10, opcode, 0b000}, (outs), (ins    R8:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, R8:$arg))]>;
    def 8ai : PI<prefix, {0b11, opcode, 0b110}, (outs), (ins i8imm:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, imm:$arg))]>;
    def 8am : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   ptr:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load   iPTR:$arg))))]>;
    def 8ao : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   off:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load offpat:$arg))))]>;
  }
  def : Pat<(!cast<SDNode>(mnemonic) A,  R8:$arg),
            (!cast<Instruction>(!strconcat(NAME, "8ar"))  R8:$arg)>;
  def : Pat<(!cast<SDNode>(mnemonic) A, imm:$arg),
            (!cast<Instruction>(!strconcat(NAME, "8ai")) imm:$arg)>;
  def : Pat<(!cast<SDNode>(mnemonic) A, (load iPTR:$arg)),
            (!cast<Instruction>(!strconcat(NAME, "8am")) ptr:$arg)>;
  def : Pat<(!cast<SDNode>(mnemonic) A, (load offpat:$arg)),
            (!cast<Instruction>(!strconcat(NAME, "8ao")) off:$arg)>;
}
multiclass BinOp8RFF<Prefix prefix, bits<3> opcode, string mnemonic,
                     SDNode node, bit compare = 0> {
  let AsmString = !strconcat(mnemonic, "\ta, $arg"),
      isCompare = compare, Defs = [A, F], Uses = [A, F] in {
    def 8ar : PI<prefix, {0b10, opcode, 0b000}, (outs), (ins    R8:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, R8:$arg, F))]>;
    def 8ai : PI<prefix, {0b11, opcode, 0b110}, (outs), (ins i8imm:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, imm:$arg, F))]>;
    def 8am : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   ptr:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load iPTR:$arg)), F))]>;
    def 8ao : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   off:$arg),
                 [(set A, F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load offpat:$arg)), F))]>;
  }
  def : Pat<(node A,  R8:$arg),
            (!cast<Instruction>(!strconcat(NAME, "8ar"))  R8:$arg)>;
  def : Pat<(node A, imm:$arg),
            (!cast<Instruction>(!strconcat(NAME, "8ai")) imm:$arg)>;
  def : Pat<(node A, (load iPTR:$arg)),
            (!cast<Instruction>(!strconcat(NAME, "8am")) ptr:$arg)>;
  def : Pat<(node A, (load offpat:$arg)),
            (!cast<Instruction>(!strconcat(NAME, "8ao")) off:$arg)>;
}
multiclass BinOp8F<Prefix prefix, bits<3> opcode, string mnemonic,
                   bit compare = 0> {
  let AsmString = !strconcat(mnemonic, "\ta, $arg"),
      isCompare = compare, Defs = [F], Uses = [A] in {
    def 8ar : PI<prefix, {0b10, opcode, 0b000}, (outs), (ins    R8:$arg),
                 [(set F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, R8:$arg))]>;
    def 8ai : PI<prefix, {0b11, opcode, 0b110}, (outs), (ins i8imm:$arg),
                 [(set F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, imm:$arg))]>;
    def 8am : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   ptr:$arg),
                 [(set F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load iPTR:$arg))))]>;
    def 8ao : PI<prefix, {0b10, opcode, 0b110}, (outs), (ins   off:$arg),
                 [(set F,
                       (!cast<SDNode>(!strconcat("Z80", mnemonic, "_flag"))
                           A, (i8 (load offpat:$arg))))]>;
  }
}
defm RLC : UnOp8RF  <CBPre, 0, "rlc">;
defm RRC : UnOp8RF  <CBPre, 1, "rrc">;
defm RL  : UnOp8RFF <CBPre, 2, "rl">;
defm RR  : UnOp8RFF <CBPre, 3, "rr">;
defm SLA : UnOp8RF  <CBPre, 4, "sla">;
defm SRA : UnOp8RF  <CBPre, 5, "sra">;
defm SRL : UnOp8RF  <CBPre, 7, "srl">;
defm INC : UnOp8RF  <NoPre, 4, "inc">;
def : Pat<(add R8:$reg, 1), (INC8r R8:$reg)>;
defm DEC : UnOp8RF  <NoPre, 5, "dec">;
def : Pat<(add R8:$reg, -1), (DEC8r R8:$reg)>;
defm ADD : BinOp8RF <NoPre, 0, "add">;
defm ADC : BinOp8RFF<NoPre, 1, "adc", adde>;
defm SUB : BinOp8RF <NoPre, 2, "sub", 1>;
defm SBC : BinOp8RFF<NoPre, 3, "sbc", sube>;
defm AND : BinOp8RF <NoPre, 4, "and">;
defm XOR : BinOp8RF <NoPre, 5, "xor">;
defm OR  : BinOp8RF <NoPre, 6, "or">;
defm CP  : BinOp8F  <NoPre, 7, "cp",  1>;
defm TST : BinOp8F  <EDPre, 4, "tst", 1>,
           Requires<[HaveEZ80Ops]>;

let Defs = [F] in {
def ADD16aa : SI<NoPre, 0x29, "add", "$dst, $src", "$src = $dst",
                 (outs A16:$dst), (ins A16:$src),
                 [(set A16:$dst, F, (Z80add_flag A16:$src, A16:$src))]>;
def ADD24aa : LI<NoPre, 0x29, "add", "$dst, $src", "$src = $dst",
                 (outs A24:$dst), (ins A24:$src),
                 [(set A24:$dst, F, (Z80add_flag A24:$src, A24:$src))]>;
def ADD16ao : SI<NoPre, 0x09, "add", "$dst, $src", "$imp = $dst",
                 (outs A16:$dst), (ins A16:$imp, O16:$src),
                 [(set A16:$dst, F, (Z80add_flag A16:$imp, O16:$src))]>;
def ADD24ao : LI<NoPre, 0x09, "add", "$dst, $src", "$imp = $dst",
                 (outs A24:$dst), (ins A24:$imp, O24:$src),
                 [(set A24:$dst, F, (Z80add_flag A24:$imp, O24:$src))]>;
let Uses = [SPS] in
def ADD16SP : SI<NoPre, 0x39, "add", "$dst, sp", "$imp = $dst",
                 (outs A16:$dst), (ins A16:$imp),
                 [(set A16:$dst, F, (Z80add_flag A16:$imp, SPS))]>;
let Uses = [SPL] in
def ADD24SP : LI<NoPre, 0x39, "add", "$dst, sp", "$imp = $dst",
                 (outs A24:$dst), (ins A24:$imp),
                 [(set A24:$dst, F, (Z80add_flag A24:$imp, SPL))]>;
}
def : Pat<(add  A16:$dst, O16:$src), (ADD16ao A16:$dst, O16:$src)>;
def : Pat<(addc A16:$dst, O16:$src), (ADD16ao A16:$dst, O16:$src)>;
def : Pat<(add  A24:$dst, O24:$src), (ADD24ao A24:$dst, O24:$src)>;
def : Pat<(addc A24:$dst, O24:$src), (ADD24ao A24:$dst, O24:$src)>;
let Defs = [HL, F] in {
let Uses = [HL, SPS, F] in {
def SBC16SP : SI<EDPre, 0x72, "sbc", "hl, sp", "", (outs), (ins),
                 [(set  HL, F, (Z80sbc_flag HL, SPS, F))]>;
def ADC16SP : SI<EDPre, 0x7A, "adc", "hl, sp", "", (outs), (ins),
                 [(set  HL, F, (Z80adc_flag HL, SPS, F))]>;
}
let Uses = [HL, F] in {
def SBC16ar : SI<EDPre, 0x42, "sbc", "hl, $src", "", (outs), (ins G16:$src),
                 [(set  HL, F, (Z80sbc_flag  HL, G16:$src, F))]>;
def ADC16ar : SI<EDPre, 0x4A, "adc", "hl, $src", "", (outs), (ins G16:$src),
                 [(set  HL, F, (Z80adc_flag  HL, G16:$src, F))]>;
}
}
let Defs = [UHL, F] in {
let Uses = [UHL, SPL, F] in {
def SBC24SP : LI<EDPre, 0x72, "sbc", "hl, sp", "", (outs), (ins),
                 [(set UHL, F, (Z80sbc_flag UHL, SPL, F))]>;
def ADC24SP : LI<EDPre, 0x7A, "adc", "hl, sp", "", (outs), (ins),
                 [(set UHL, F, (Z80adc_flag UHL, SPL, F))]>;
}
let Uses = [UHL, F] in {
def SBC24ar : LI<EDPre, 0x42, "sbc", "hl, $src", "", (outs), (ins G24:$src),
                 [(set UHL, F, (Z80sbc_flag UHL, G24:$src, F))]>;
def ADC24ar : LI<EDPre, 0x4A, "adc", "hl, $src", "", (outs), (ins G24:$src),
                 [(set UHL, F, (Z80adc_flag UHL, G24:$src, F))]>;
}
}
def : Pat<(sube  HL, G16:$src), (SBC16ar G16:$src)>;
def : Pat<(adde  HL, G16:$src), (ADC16ar G16:$src)>;
def : Pat<(sube UHL, G24:$src), (SBC24ar G24:$src)>;
def : Pat<(adde UHL, G24:$src), (ADC24ar G24:$src)>;

let Defs = [F] in {
def INC16r : SI<NoPre, 0x03, "inc", "$dst", "$dst = $imp",
                (outs R16:$dst), (ins R16:$imp),
                [(set R16:$dst, F, (Z80inc_flag R16:$imp))]>;
def DEC16r : SI<NoPre, 0x0B, "dec", "$dst", "$dst = $imp",
                (outs R16:$dst), (ins R16:$imp),
                [(set R16:$dst, F, (Z80dec_flag R16:$imp))]>;
def INC24r : LI<NoPre, 0x03, "inc", "$dst", "$dst = $imp",
                (outs R24:$dst), (ins R24:$imp),
                [(set R24:$dst, F, (Z80inc_flag R24:$imp))]>;
def DEC24r : LI<NoPre, 0x0B, "dec", "$dst", "$dst = $imp",
                (outs R24:$dst), (ins R24:$imp),
                [(set R24:$dst, F, (Z80dec_flag R24:$imp))]>;
}
def : Pat<(add R16:$reg,  1), (INC16r R16:$reg)>;
def : Pat<(add R16:$reg, -1), (DEC16r R16:$reg)>;
def : Pat<(add R24:$reg,  1), (INC24r R24:$reg)>;
def : Pat<(add R24:$reg, -1), (DEC24r R24:$reg)>;

def LEA16ro : SI<EDPre, 0x02, "lea", "$dst, $src", "",
                 (outs R16:$dst), (ins off16:$src),
                 [(set R16:$dst, offpat:$src)]>, Requires<[HaveEZ80Ops]>;
def LEA24ro : LI<EDPre, 0x02, "lea", "$dst, $src", "",
                 (outs R24:$dst), (ins off24:$src),
                 [(set R24:$dst, offpat:$src)]>;

let AsmString = "mlt\t$dst", Constraints = "$src = $dst" in
def MLT8rr : PI<EDPre, 0x4C, (outs G16:$dst), (ins G16:$src),
                [(set G16:$dst, (Z80mlt G16:$src))]>, Requires<[HaveZ180Ops]>;

//===----------------------------------------------------------------------===//
// Non-Instruction Patterns.
//===----------------------------------------------------------------------===//

// ConstantPool, GlobalAddress, ExternalSymbol, and JumpTable
//def : Pat<(i16 (Z80Wrapper tglobaladdr:$src)), (LD16ri tglobaladdr:$src)>;
//def : Pat<(i24 (Z80Wrapper tglobaladdr:$src)), (LD24ri tglobaladdr:$src)>;

// calls
//def : Pat<(Z80call (tglobaladdr :$target)), (CALL16im tglobaladdr :$target)>;
//def : Pat<(Z80call (texternalsym:$target)), (CALL16im texternalsym:$target)>;
//def : Pat<(Z80call (tglobaladdr :$target)), (CALL24im tglobaladdr :$target)>;
//def : Pat<(Z80call (texternalsym:$target)), (CALL24im texternalsym:$target)>;

//===----------------------------------------------------------------------===//
// Subsystems.
//===----------------------------------------------------------------------===//

//def : Pat<(i24 (Z80sub_flag UHL, G24:$src)), (SBC24ar G24:$src, RCF)>;

// mlt
//def : Pat<(i8 (mul R8:$arg1, R8:$arg2)),
//          (SUBREG_TO_REG (i24 0), (MLT8rr (INSERT_SUBREG R8:$arg1)), sub_low)>;

/*def : Pat<(i32 (imm:$src)),
          (REG_SEQUENCE R32, (LD24ri (imm_long_XFORM imm:$src)), sub_long,
                             (LD8ri (imm_top_XFORM imm:$src)), sub_top)>;*/

// anyexts
def : Pat<(i16 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i24 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i24 (anyext R16:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R16:$src, sub_short)>;
/*def : Pat<(i32 (anyext R8 :$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R8 :$src, sub_low)>;
def : Pat<(i32 (anyext R16:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R16:$src, sub_short)>;
def : Pat<(i32 (anyext R24:$src)),
          (INSERT_SUBREG (IMPLICIT_DEF), R24:$src, sub_long)>;*/

// zexts
def : Pat<(i16 (zext R8 :$src)),
          (REG_SEQUENCE R16, R8 :$src, sub_low, (LD8ri 0), sub_high)>;
def : Pat<(i24 (zext R8 :$src)),
          (INSERT_SUBREG (LD24ri 0), R8 :$src, sub_low)>;
def : Pat<(i24 (zext R16:$src)),
          (INSERT_SUBREG (LD24ri 0), R16:$src, sub_short)>;
/*def : Pat<(i32 (zext R8:$src)),
          (REG_SEQUENCE R32, (INSERT_SUBREG (LD24ri 0), R8:$src,
                                            sub_low), sub_long,
                             (LD8ri 0), sub_top)>;
def : Pat<(i32 (zext R16:$src)),
          (REG_SEQUENCE R32, (INSERT_SUBREG (LD24ri 0), R16:$src,
                                            sub_short), sub_long,
                             (LD8ri 0), sub_top)>;
def : Pat<(i32 (zext R24:$src)),
          (REG_SEQUENCE R32, R24:$src, sub_long, (LD8ri 0), sub_top)>;*/

// truncs
def : Pat<(i8  (trunc R16:$src)), (EXTRACT_SUBREG R16:$src, sub_low)>;
def : Pat<(i8  (trunc R24:$src)), (EXTRACT_SUBREG R24:$src, sub_low)>;
def : Pat<(i16 (trunc R24:$src)), (EXTRACT_SUBREG R24:$src, sub_short)>;
/*def : Pat<(i8  (trunc R32:$src)), (EXTRACT_SUBREG R32:$src, sub_low)>;
def : Pat<(i16 (trunc R32:$src)), (EXTRACT_SUBREG R32:$src, sub_short)>;
def : Pat<(i24 (trunc R32:$src)), (EXTRACT_SUBREG R32:$src, sub_long)>;*/

// loads
//def : Pat<(i32 (load mempat:$src)), (REG_SEQUENCE R32, (LD24rm mempat:$src), sub_long, (LD8am (imm_add_3_XFORM mempat:$src)), sub_top)>;
//def : Pat<(i32 (load   iPTR:$src)), (REG_SEQUENCE R32, (LD24rp   iPTR:$src), sub_long, (LD8rp (fi_add_3_XFORM iPTR:$src)), sub_top)>;
//def : Pat<(i32 (load offpat:$src)), (REG_SEQUENCE R32, (LD24ro offpat:$src), sub_long, (LD8ro offpat:$src), sub_top)>;
//def : Pat<(i32 (load iPTR:$src)), (REG_SEQUENCE R32, (LD8ri 0), sub_top, (LD24rp iPTR:$src), sub_long)>;
//def : Pat<(i32 (load iPTR:$src)), (REG_SEQUENCE R32, (R8 (LD8rp (imm_add_3_XFORM iPTR:$src))), sub_top,
//                                                     (R24 (LD24rp iPTR:$src)), sub_long)>;

// /*
// // Any instruction that defines a 16-bit result leaves the high half of the
// // register zero. Truncate can be lowered to EXTRACT_SUBREG. CopyFromReg may
// // be copying from a truncate. But any other 16-bit operation will zero-extend
// // up to 24 bits.
// def def16 : PatLeaf<(i16 R16:$src), [{
//   dbgs() << "Hello World!\n";
//   return N->getOpcode() != ISD::TRUNCATE &&
//          N->getOpcode() != TargetOpcode::EXTRACT_SUBREG &&
//          N->getOpcode() != ISD::CopyFromReg;
// }]>;

// // In the case of a 16-bit def that is known to implicitly zero-extend,
// // we can use a SUBREG_TO_REG.
// def : Pat<(i24 (zext def16:$src)),
//           (SUBREG_TO_REG (i24 0), R16:$src, sub_short)>;
// */

// // Compiler Pseudo Instructions and Pat Patterns
// //include "Z80InstrCompiler.td"
