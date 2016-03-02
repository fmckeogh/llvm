//===-- Z80.td - Target definition file for the Zilog Z80 --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a target description file for the Zilog z80 architecture, referred
// to here as the "Z80" architecture.
//
//===----------------------------------------------------------------------===//

// Get the target-independent interfaces which we are implementing...
//
include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Z80 Subtarget state
//

def Mode24Bit : SubtargetFeature<"24bit-mode", "In24BitMode", "true",
                                  "24-bit mode (ez80)">;
def Mode16Bit : SubtargetFeature<"16bit-mode", "In16BitMode", "true",
                                  "16-bit mode (z80)">;

//===----------------------------------------------------------------------===//
// Register File Description
//===----------------------------------------------------------------------===//

include "Z80RegisterInfo.td"

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

include "Z80InstrInfo.td"

def Z80InstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// Calling Conventions
//===----------------------------------------------------------------------===//

include "Z80CallingConv.td"


def Z80 : Target {
  // Information about the instructions...
  let InstructionSet = Z80InstrInfo;
}
