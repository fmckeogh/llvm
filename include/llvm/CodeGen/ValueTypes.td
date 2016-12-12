//===- ValueTypes.td - ValueType definitions ---------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Value types - These values correspond to the register types defined in the
// ValueTypes.h file.  If you update anything here, you must update it there as
// well!
//
//===----------------------------------------------------------------------===//

class ValueType<int size, int value> {
  string Namespace = "MVT";
  int Size = size;
  int Value = value;
}

def OtherVT: ValueType<0  ,  0>;   // "Other" value
def i1     : ValueType<1  ,  1>;   // One bit boolean value
def i8     : ValueType<8  ,  2>;   // 8-bit integer value
def i16    : ValueType<16 ,  3>;   // 16-bit integer value
def i24    : ValueType<24 ,  4>;   // 24-bit integer value
def i32    : ValueType<32 ,  5>;   // 32-bit integer value
def i48    : ValueType<48 ,  6>;   // 48-bit integer value
def i64    : ValueType<64 ,  7>;   // 64-bit integer value
def i96    : ValueType<96 ,  8>;   // 96-bit integer value
def i128   : ValueType<128,  9>;   // 128-bit integer value
def f16    : ValueType<16 , 10>;   // 16-bit floating point value
def f32    : ValueType<32 , 11>;   // 32-bit floating point value
def f64    : ValueType<64 , 12>;   // 64-bit floating point value
def f80    : ValueType<80 , 13>;   // 80-bit floating point value
def f128   : ValueType<128, 14>;   // 128-bit floating point value
def ppcf128: ValueType<128, 15>;   // PPC 128-bit floating point value

def v2i1   : ValueType<2 ,  16>;   //   2 x i1 vector value
def v4i1   : ValueType<4 ,  17>;   //   4 x i1 vector value
def v8i1   : ValueType<8 ,  18>;   //   8 x i1 vector value
def v16i1  : ValueType<16,  19>;   //  16 x i1 vector value
def v32i1  : ValueType<32 , 20>;   //  32 x i1 vector value
def v64i1  : ValueType<64 , 21>;   //  64 x i1 vector value
def v512i1 : ValueType<512, 22>;   // 512 x i1 vector value
def v1024i1: ValueType<1024,23>;   //1024 x i1 vector value

def v1i8   : ValueType<16,  24>;   //  1 x i8  vector value
def v2i8   : ValueType<16 , 25>;   //  2 x i8  vector value
def v4i8   : ValueType<32 , 26>;   //  4 x i8  vector value
def v8i8   : ValueType<64 , 27>;   //  8 x i8  vector value
def v16i8  : ValueType<128, 28>;   // 16 x i8  vector value
def v32i8  : ValueType<256, 29>;   // 32 x i8  vector value
def v64i8  : ValueType<512, 30>;   // 64 x i8  vector value
def v128i8 : ValueType<1024,31>;   //128 x i8  vector value
def v256i8 : ValueType<2048,32>;   //256 x i8  vector value

def v1i16  : ValueType<16 , 33>;   //  1 x i16 vector value
def v2i16  : ValueType<32 , 34>;   //  2 x i16 vector value
def v4i16  : ValueType<64 , 35>;   //  4 x i16 vector value
def v8i16  : ValueType<128, 36>;   //  8 x i16 vector value
def v16i16 : ValueType<256, 37>;   // 16 x i16 vector value
def v32i16 : ValueType<512, 38>;   // 32 x i16 vector value
def v64i16 : ValueType<1024,39>;   // 64 x i16 vector value
def v128i16: ValueType<2048,40>;   //128 x i16 vector value

def v1i32  : ValueType<32 , 41>;   //  1 x i32 vector value
def v2i32  : ValueType<64 , 42>;   //  2 x i32 vector value
def v4i32  : ValueType<128, 43>;   //  4 x i32 vector value
def v8i32  : ValueType<256, 44>;   //  8 x i32 vector value
def v16i32 : ValueType<512, 45>;   // 16 x i32 vector value
def v32i32 : ValueType<1024,46>;   // 32 x i32 vector value
def v64i32 : ValueType<2048,47>;   // 32 x i32 vector value

def v1i64  : ValueType<64 , 48>;   //  1 x i64 vector value
def v2i64  : ValueType<128, 49>;   //  2 x i64 vector value
def v4i64  : ValueType<256, 50>;   //  4 x i64 vector value
def v8i64  : ValueType<512, 51>;   //  8 x i64 vector value
def v16i64 : ValueType<1024,52>;   // 16 x i64 vector value
def v32i64 : ValueType<2048,53>;   // 32 x i64 vector value

def v1i128 : ValueType<128, 54>;   //  1 x i128 vector value

def v2f16  : ValueType<32 , 55>;   //  2 x f16 vector value
def v4f16  : ValueType<64 , 56>;   //  4 x f16 vector value
def v8f16  : ValueType<128, 57>;   //  8 x f16 vector value
def v1f32  : ValueType<32 , 58>;   //  1 x f32 vector value
def v2f32  : ValueType<64 , 59>;   //  2 x f32 vector value
def v4f32  : ValueType<128, 60>;   //  4 x f32 vector value
def v8f32  : ValueType<256, 61>;   //  8 x f32 vector value
def v16f32 : ValueType<512, 62>;   // 16 x f32 vector value
def v1f64  : ValueType<64,  63>;   //  1 x f64 vector value
def v2f64  : ValueType<128, 64>;   //  2 x f64 vector value
def v4f64  : ValueType<256, 65>;   //  4 x f64 vector value
def v8f64  : ValueType<512, 66>;   //  8 x f64 vector value

def x86mmx : ValueType<64 , 67>;   // X86 MMX value
def FlagVT : ValueType<0  , 68>;   // Pre-RA sched glue
def isVoid : ValueType<0  , 69>;   // Produces no value
def untyped: ValueType<8  , 70>;   // Produces an untyped value
def token  : ValueType<0  , 120>;  // TokenTy
def MetadataVT: ValueType<0, 121>; // Metadata

// Pseudo valuetype mapped to the current pointer size to any address space.
// Should only be used in TableGen.
def iPTRAny   : ValueType<0, 122>;

// Pseudo valuetype to represent "vector of any size"
def vAny   : ValueType<0  , 123>;

// Pseudo valuetype to represent "float of any format"
def fAny   : ValueType<0  , 124>;

// Pseudo valuetype to represent "integer of any bit width"
def iAny   : ValueType<0  , 125>;

// Pseudo valuetype mapped to the current pointer size.
def iPTR   : ValueType<0  , 126>;

// Pseudo valuetype to represent "any type of any size".
def Any    : ValueType<0  , 127>;
