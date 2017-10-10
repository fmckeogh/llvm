//===- OMF.h - OMF object file implementation -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the OMFObjectFile class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_OBJECT_OMF_H
#define LLVM_OBJECT_OMF_H

#include "llvm/ADT/MapVector.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Endian.h"

namespace llvm {
namespace object {

// OMF Record IDs
constexpr StringLiteral
  OMF_AssignValToVar("\xE2\xD7"),
  OMF_DefineContext("\xFB");

// OMF Record headers
enum {
  OMF_COMMA  = 0x90, ///< ,
  OMF_FALSE  = 0xA0, ///< @F
  OMF_TRUE   = 0xA1, ///< @T
  OMF_ABS    = 0xA2, ///< @ABS
  OMF_NEG    = 0xA3, ///< @NEG
  OMF_NOT    = 0xA4, ///< @NOT
  OMF_ADD    = 0xA5, ///< +
  OMF_SUB    = 0xA6, ///< -
  OMF_DIV    = 0xA7, ///< /
  OMF_MUL    = 0xA8, ///< *
  OMF_MAX    = 0xA9, ///< @MAX
  OMF_MIN    = 0xAA, ///< @MIN
  OMF_MOD    = 0xAB, ///< @MOD
  OMF_LTHAN  = 0xAC, ///< <
  OMF_GTHAN  = 0xAD, ///< >
  OMF_EQUAL  = 0xAE, ///< =
  OMF_NEQUAL = 0xAF, ///< != or <>
  OMF_AND    = 0xB0, ///< @AND
  OMF_OR     = 0xB1, ///< @OR
  OMF_XOR    = 0xB2, ///< @XOR
  OMF_EXT    = 0xB3, ///< @EXT
  OMF_INS    = 0xB4, ///< @INS
  OMF_ERR    = 0xB5, ///< @ERR
  OMF_IF     = 0xB6, ///< @IF
  OMF_ELSE   = 0xB7, ///< @ELSE
  OMF_END    = 0xB8, ///< @END
  OMF_ESC    = 0xB9, ///< @ESCAPE
  OMF_LBRACK = 0xBA, ///< [
  OMF_RBRACK = 0xBB, ///< ]
  OMF_LBRACE = 0xBC, ///< {
  OMF_RBRACE = 0xBD, ///< }
  OMF_LPAREN = 0xBE, ///< (
  OMF_RPAREN = 0xBF, ///< )
  OMF_NULL = 0xC0,
  OMF_A = OMF_NULL|'A', ///< Type B Section Physical Address
  OMF_B = OMF_NULL|'B', ///< Type B Section Size
  OMF_F = OMF_NULL|'F', ///< Section AMU Size
  OMF_G = OMF_NULL|'G', ///< Execution starting address.
  OMF_I = OMF_NULL|'I', ///< Address of public symbol n.
  OMF_L = OMF_NULL|'L', ///< The logical address of a section n.
  OMF_M = OMF_NULL|'M', ///< The most significant half of a two part logical
                        ///< address of a section n.
  OMF_N = OMF_NULL|'N', ///< Address of local symbol n.
  OMF_P = OMF_NULL|'P', ///< The program counter for section n.
  OMF_R = OMF_NULL|'R',
  OMF_S = OMF_NULL|'S', ///< The size, in MAUs, ,of a section n.
  OMF_W = OMF_NULL|'W', ///< The file offset, in bytes, of the nth part of the
                        ///< object file.
  OMF_X = OMF_NULL|'X', ///< Address of external symbol n.
  OMF_Z = OMF_NULL|'Z',
  OMF_EL1 = 0xDE, ///< Extension Length
  OMF_EL2 = 0xDF, ///< Extension Length
  OMF_FIRST_RECORD = 0xE0,
  OMF_RECORD_MB = 0xE0, ///< Module Beginning
  OMF_RECORD_ME = 0xE1, ///< Module End
  OMF_RECORD_AS = 0xE2, ///< Assign
  OMF_RECORD_IR = 0xE3, ///< Initialize Relocation Base
  OMF_RECORD_LR = 0xE4, ///< Load With Relocation
  OMF_RECORD_SB = 0xE5, ///< Current Section
  OMF_RECORD_ST = 0xE6, ///< Section Type
  OMF_RECORD_SA = 0xE7, ///< Section Alignment
  OMF_RECORD_NI = 0xE8, ///< Public (External) Symbol
  OMF_RECORD_NX = 0xE9, ///< External Reference Name
  OMF_RECORD_AD = 0xEC, ///< Address Descriptor
  OMF_RECORD_LD = 0xED, ///< Load Constant MAUs
  OMF_RECORD_C1 = 0xEE, ///< Checksum Record
  OMF_RECORD_C2 = 0xEF, ///< Checksum Record
  OMF_RECORD_NN = 0xF0, ///< Variable Attributes
  OMF_RECORD_AT = 0xF1, ///< Variable Attributes
  OMF_RECORD_TY = 0xF2, ///< Define Type Characteristics
  OMF_RECORD_WX = 0xF4, ///< Weak External Reference
  OMF_RECORD_RE = 0xF7, ///< Repeat Data
  OMF_RECORD_BB = 0xF8, ///< Declare Block Beginning
  OMF_RECORD_BE = 0xF9, ///< Declare Block End
  OMF_RECORD_LT = 0xFA, ///< Load With Translation
  OMF_RECORD_NC = 0xFB, ///< Define Context
};

// OMF Parts
enum {
  OMF_ADExtensionPart,
  OMF_EnvironmentPart,
  OMF_SectionPart,
  OMF_ExternalPart,
  OMF_DebugInfoPart,
  OMF_DataPart,
  OMF_TrailerPart,
  OMF_ModuleEnd,
  OMF_NumParts
};

// OMF Object Format Types
enum {
  OMF_Absolute = 1,
  OMF_Relocatable,
  OMF_Loadable,
  OMF_Library
};

// OMF Memory Models
enum {
  OMF_Tiny,    ///< Code and data are in the same single 64K segment/page.
  OMF_Small,   ///< Code and data each have a single 64K segment/page.
  OMF_Medium,  ///< Data has a single 64K segment/page, while code has multiple
               ///    64K segments/pages.
  OMF_Compact, ///< Data has multiple 64K segments/pages, while code has a
               ///    single 64K segment/page.
  OMF_Large,   ///< Both data and code have multiple 64K segments/pages.
  OMF_Big,     ///< Code has multiple 64K segments/pages, while there is a
               ///    common "near" data area with far data areas available;
               ///    normally and stack are together.
  OMF_Huge     ///< All large arrays and structures are in their own section so
               ///    that addressing involves computations (you can have arrays
               ///    and structures bigger than 64K).
};

typedef StringRef OMFContext;

struct OMFSection {
  StringRef Type, Name;
  uint64_t Parent = 0, Brother = 0, Context = 0;
  uint64_t Alignment = 0, PageSize = 0, Size = 0;
  std::string Data;
};

struct OMFSymbol {
  uint32_t Flags = BasicSymbolRef::SF_None;
  StringRef Name;
  uint64_t Section = 0, Address = 0;
};

} // end namespace object
} // end namespace llvm

#endif // LLVM_OBJECT_OMF_H
