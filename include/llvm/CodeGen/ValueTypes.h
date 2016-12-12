//===- CodeGen/ValueTypes.h - Low-Level Target independ. types --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the set of low-level target independent types which various
// values in the code generator are.  This allows the target specific behavior
// of instructions to be described to target independent passes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_VALUETYPES_H
#define LLVM_CODEGEN_VALUETYPES_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/CodeGen/MachineValueType.h"
#include <cassert>
#include <string>

namespace llvm {

  class LLVMContext;
  class Type;

  /// EVT - Extended Value Type.  Capable of holding value types which are not
  /// native for any processor (such as the i12345 type), as well as the types
  /// a MVT can represent.
  struct EVT {
  private:
    MVT V;
    Type *LLVMTy;

  public:
    constexpr EVT() : V(MVT::INVALID_SIMPLE_VALUE_TYPE), LLVMTy(nullptr) {}
    constexpr EVT(MVT::SimpleValueType SVT) : V(SVT), LLVMTy(nullptr) {}
    constexpr EVT(MVT S) : V(S), LLVMTy(nullptr) {}

    bool operator==(EVT VT) const {
      return !(*this != VT);
    }
    bool operator!=(EVT VT) const {
      if (V.SimpleTy != VT.V.SimpleTy)
        return true;
      if (V.SimpleTy < 0)
        return LLVMTy != VT.LLVMTy;
      return false;
    }

    /// getFloatingPointVT - Returns the EVT that represents a floating point
    /// type with the given number of bits.  There are two floating point types
    /// with 128 bits - this returns f128 rather than ppcf128.
    static EVT getFloatingPointVT(unsigned BitWidth) {
      return MVT::getFloatingPointVT(BitWidth);
    }

    /// getIntegerVT - Returns the EVT that represents an integer with the given
    /// number of bits.
    static EVT getIntegerVT(LLVMContext &Context, unsigned BitWidth) {
      MVT M = MVT::getIntegerVT(BitWidth);
      if (M.SimpleTy >= 0)
        return M;
      return getExtendedIntegerVT(Context, BitWidth);
    }

    /// getVectorVT - Returns the EVT that represents a vector NumElements in
    /// length, where each element is of type VT.
    static EVT getVectorVT(LLVMContext &Context, EVT VT, unsigned NumElements) {
      MVT M = MVT::getVectorVT(VT.V, NumElements);
      if (M.SimpleTy >= 0)
        return M;
      return getExtendedVectorVT(Context, VT, NumElements);
    }

    /// changeVectorElementTypeToInteger - Return a vector with the same number
    /// of elements as this vector, but with the element type converted to an
    /// integer type with the same bitwidth.
    EVT changeVectorElementTypeToInteger() const {
      if (!isSimple())
        return changeExtendedVectorElementTypeToInteger();
      MVT EltTy = getSimpleVT().getVectorElementType();
      unsigned BitWidth = EltTy.getSizeInBits();
      MVT IntTy = MVT::getIntegerVT(BitWidth);
      MVT VecTy = MVT::getVectorVT(IntTy, getVectorNumElements());
      assert(VecTy.SimpleTy >= 0 &&
             "Simple vector VT not representable by simple integer vector VT!");
      return VecTy;
    }

    /// changeTypeToInteger - Return the type converted to an equivalently sized
    /// integer or vector with integer element type. Similar to
    /// changeVectorElementTypeToInteger, but also handles scalars.
    EVT changeTypeToInteger() const {
      if (isVector())
        return changeVectorElementTypeToInteger();

      if (isSimple())
        return MVT::getIntegerVT(getSizeInBits());

      return changeExtendedTypeToInteger();
    }

    /// changeTypeToScalarInteger - Return the type converted to an equivalently
    /// sized integer. Similar to changeTypeToInteger, but vectors are converted
    /// to a scalar integer.
    EVT changeTypeToScalarInteger(LLVMContext &Context) const {
      return getIntegerVT(Context, getSizeInBits());
    }

    /// isSimple - Test if the given EVT is simple (as opposed to being
    /// extended).
    bool isSimple() const {
      return V.SimpleTy >= 0;
    }

    /// isExtended - Test if the given EVT is extended (as opposed to
    /// being simple).
    bool isExtended() const {
      return !isSimple();
    }

    /// isFloatingPoint - Return true if this is a FP, or a vector FP type.
    bool isFloatingPoint() const {
      return isSimple() ? V.isFloatingPoint() : isExtendedFloatingPoint();
    }

    /// isInteger - Return true if this is an integer, or a vector integer type.
    bool isInteger() const {
      return isSimple() ? V.isInteger() : isExtendedInteger();
    }

    /// isScalarInteger - Return true if this is an integer, but not a vector.
    bool isScalarInteger() const {
      return isSimple() ? V.isScalarInteger() : isExtendedScalarInteger();
    }

    /// isVector - Return true if this is a vector value type.
    bool isVector() const {
      return isSimple() ? V.isVector() : isExtendedVector();
    }

    /// is16BitVector - Return true if this is a 16-bit vector type.
    bool is16BitVector() const {
      return isSimple() ? V.is16BitVector() : isExtended16BitVector();
    }

    /// is32BitVector - Return true if this is a 32-bit vector type.
    bool is32BitVector() const {
      return isSimple() ? V.is32BitVector() : isExtended32BitVector();
    }

    /// is64BitVector - Return true if this is a 64-bit vector type.
    bool is64BitVector() const {
      return isSimple() ? V.is64BitVector() : isExtended64BitVector();
    }

    /// is128BitVector - Return true if this is a 128-bit vector type.
    bool is128BitVector() const {
      return isSimple() ? V.is128BitVector() : isExtended128BitVector();
    }

    /// is256BitVector - Return true if this is a 256-bit vector type.
    bool is256BitVector() const {
      return isSimple() ? V.is256BitVector() : isExtended256BitVector();
    }

    /// is512BitVector - Return true if this is a 512-bit vector type.
    bool is512BitVector() const {
      return isSimple() ? V.is512BitVector() : isExtended512BitVector();
    }

    /// is1024BitVector - Return true if this is a 1024-bit vector type.
    bool is1024BitVector() const {
      return isSimple() ? V.is1024BitVector() : isExtended1024BitVector();
    }

    /// is2048BitVector - Return true if this is a 2048-bit vector type.
    bool is2048BitVector() const {
      return isSimple() ? V.is2048BitVector() : isExtended2048BitVector();
    }

    /// isOverloaded - Return true if this is an overloaded type for TableGen.
    bool isOverloaded() const {
      return (V==MVT::iAny || V==MVT::fAny || V==MVT::vAny || V==MVT::iPTRAny);
    }

    /// isByteSized - Return true if the bit size is a multiple of 8.
    bool isByteSized() const {
      return (getSizeInBits() & 7) == 0;
    }

    /// isRound - Return true if the size is a power-of-two number of bytes.
    bool isRound() const {
      unsigned BitSize = getSizeInBits();
      return BitSize >= 8 && !(BitSize & (BitSize - 1));
    }

    /// bitsEq - Return true if this has the same number of bits as VT.
    bool bitsEq(EVT VT) const {
      if (EVT::operator==(VT)) return true;
      return getSizeInBits() == VT.getSizeInBits();
    }

    /// bitsGT - Return true if this has more bits than VT.
    bool bitsGT(EVT VT) const {
      if (EVT::operator==(VT)) return false;
      return getSizeInBits() > VT.getSizeInBits();
    }

    /// bitsGE - Return true if this has no less bits than VT.
    bool bitsGE(EVT VT) const {
      if (EVT::operator==(VT)) return true;
      return getSizeInBits() >= VT.getSizeInBits();
    }

    /// bitsLT - Return true if this has less bits than VT.
    bool bitsLT(EVT VT) const {
      if (EVT::operator==(VT)) return false;
      return getSizeInBits() < VT.getSizeInBits();
    }

    /// bitsLE - Return true if this has no more bits than VT.
    bool bitsLE(EVT VT) const {
      if (EVT::operator==(VT)) return true;
      return getSizeInBits() <= VT.getSizeInBits();
    }


    /// getSimpleVT - Return the SimpleValueType held in the specified
    /// simple EVT.
    MVT getSimpleVT() const {
      assert(isSimple() && "Expected a SimpleValueType!");
      return V;
    }

    /// getScalarType - If this is a vector type, return the element type,
    /// otherwise return this.
    EVT getScalarType() const {
      return isVector() ? getVectorElementType() : *this;
    }

    /// getVectorElementType - Given a vector type, return the type of
    /// each element.
    EVT getVectorElementType() const {
      assert(isVector() && "Invalid vector type!");
      if (isSimple())
        return V.getVectorElementType();
      return getExtendedVectorElementType();
    }

    /// getVectorNumElements - Given a vector type, return the number of
    /// elements it contains.
    unsigned getVectorNumElements() const {
      assert(isVector() && "Invalid vector type!");
      if (isSimple())
        return V.getVectorNumElements();
      return getExtendedVectorNumElements();
    }

    /// getSizeInBits - Return the size of the specified value type in bits.
    unsigned getSizeInBits() const {
      if (isSimple())
        return V.getSizeInBits();
      return getExtendedSizeInBits();
    }

    /// getNumParts - Return the number of parts with PartBits bits that make up
    /// this VT.
    unsigned getNumParts(unsigned PartBits) const {
      return (getSizeInBits() + PartBits - 1) / PartBits;
    }

    /// getNumParts - Return the number of parts of type PartVT that make up
    /// this VT.
    unsigned getNumParts(EVT PartVT) const {
      return getNumParts(PartVT.getSizeInBits());
    }

    unsigned getScalarSizeInBits() const {
      return getScalarType().getSizeInBits();
    }

    /// getStoreSize - Return the number of bytes overwritten by a store
    /// of the specified value type.
    unsigned getStoreSize() const {
      return getNumParts(8);
    }

    /// getStoreSizeInBits - Return the number of bits overwritten by a store
    /// of the specified value type.
    unsigned getStoreSizeInBits() const {
      return getStoreSize() * 8;
    }

    /// getRoundIntegerType - Rounds the bit-width of the given integer EVT up
    /// to the nearest power of two (and at least to eight), and returns the
    /// integer EVT with that number of bits.
    EVT getRoundIntegerType(LLVMContext &Context) const {
      assert(isScalarInteger() && "Invalid integer type!");
      unsigned BitWidth = getSizeInBits();
      if (BitWidth <= 8)
        return EVT(MVT::i8);
      return getIntegerVT(Context, 1 << Log2_32_Ceil(BitWidth));
    }

    /// getHalfSizedIntegerVT - Finds the smallest simple value type that is
    /// greater than or equal to half the width of this EVT. If no simple
    /// value type can be found, an extended integer value type of half the
    /// size (rounded up) is returned.
    EVT getHalfSizedIntegerVT(LLVMContext &Context) const {
      assert(isScalarInteger() && "Invalid integer type!");
      unsigned EVTSize = getSizeInBits();
      for (MVT HalfVT : MVT::integer_valuetypes())
        if (HalfVT.getSizeInBits() * 2 >= getSizeInBits())
          return HalfVT;
      return getIntegerVT(Context, (EVTSize + 1) / 2);
    }

    /// \brief Return a VT for an integer vector type with the size of the
    /// elements doubled. The typed returned may be an extended type.
    EVT widenIntegerVectorElementType(LLVMContext &Context) const {
      EVT EltVT = getVectorElementType();
      EltVT = EVT::getIntegerVT(Context, 2 * EltVT.getSizeInBits());
      return EVT::getVectorVT(Context, EltVT, getVectorNumElements());
    }

    /// isPow2Size - Returns true if the size of the EVT is a power of 2.
    bool isPow2Size() const {
      unsigned BitWidth = getSizeInBits();
      return !(BitWidth & (BitWidth - 1));
    }

    /// isPow2VectorType - Returns true if the given vector is a power of 2.
    bool isPow2VectorType() const {
      unsigned NElts = getVectorNumElements();
      return !(NElts & (NElts - 1));
    }

    /// getPow2VectorType - Widens the length of the given vector EVT up to
    /// the nearest power of 2 and returns that type.
    EVT getPow2VectorType(LLVMContext &Context) const {
      if (!isPow2VectorType()) {
        unsigned NElts = getVectorNumElements();
        unsigned Pow2NElts = 1 <<  Log2_32_Ceil(NElts);
        return EVT::getVectorVT(Context, getVectorElementType(), Pow2NElts);
      }
      else {
        return *this;
      }
    }

    /// getEVTString - This function returns value type as a string,
    /// e.g. "i32".
    std::string getEVTString() const;

    /// getTypeForEVT - This method returns an LLVM type corresponding to the
    /// specified EVT.  For integer types, this returns an unsigned type.  Note
    /// that this will abort for types that cannot be represented.
    Type *getTypeForEVT(LLVMContext &Context) const;

    /// getEVT - Return the value type corresponding to the specified type.
    /// This returns all pointers as iPTR.  If HandleUnknown is true, unknown
    /// types are returned as Other, otherwise they are invalid.
    static EVT getEVT(Type *Ty, bool HandleUnknown = false);

    intptr_t getRawBits() const {
      if (isSimple())
        return V.SimpleTy;
      else
        return (intptr_t)(LLVMTy);
    }

    /// compareRawBits - A meaningless but well-behaved order, useful for
    /// constructing containers.
    struct compareRawBits {
      bool operator()(EVT L, EVT R) const {
        if (L.V.SimpleTy == R.V.SimpleTy)
          return L.LLVMTy < R.LLVMTy;
        else
          return L.V.SimpleTy < R.V.SimpleTy;
      }
    };

  private:
    // Methods for handling the Extended-type case in functions above.
    // These are all out-of-line to prevent users of this header file
    // from having a dependency on Type.h.
    EVT changeExtendedTypeToInteger() const;
    EVT changeExtendedVectorElementTypeToInteger() const;
    static EVT getExtendedIntegerVT(LLVMContext &C, unsigned BitWidth);
    static EVT getExtendedVectorVT(LLVMContext &C, EVT VT,
                                   unsigned NumElements);
    bool isExtendedFloatingPoint() const LLVM_READONLY;
    bool isExtendedInteger() const LLVM_READONLY;
    bool isExtendedScalarInteger() const LLVM_READONLY;
    bool isExtendedVector() const LLVM_READONLY;
    bool isExtended16BitVector() const LLVM_READONLY;
    bool isExtended32BitVector() const LLVM_READONLY;
    bool isExtended64BitVector() const LLVM_READONLY;
    bool isExtended128BitVector() const LLVM_READONLY;
    bool isExtended256BitVector() const LLVM_READONLY;
    bool isExtended512BitVector() const LLVM_READONLY;
    bool isExtended1024BitVector() const LLVM_READONLY;
    bool isExtended2048BitVector() const LLVM_READONLY;
    EVT getExtendedVectorElementType() const;
    unsigned getExtendedVectorNumElements() const LLVM_READONLY;
    unsigned getExtendedSizeInBits() const LLVM_READONLY;
  };

  /// VTS - Value Type Sequence.  Represents a sequence of either one type
  /// repeated, or two related types alternating.  The one repeated type case is
  /// represented as both types being identical.
  template<typename VT>
  struct VTS {
  private:
    VT VTs[2];

  public:
    constexpr VTS() {}
    VTS(MVT::SimpleValueType SVT) : VTS(MVT(SVT), MVT(SVT)) {}
    VTS(MVT::SimpleValueType FirstSVT, MVT::SimpleValueType SecondSVT)
        : VTS(MVT(FirstSVT), MVT(SecondSVT)) {}
    VTS(VT SingleVT) : VTS(SingleVT, SingleVT) {}
    VTS(VT FirstVT, VT SecondVT) : VTs{FirstVT, SecondVT} {
      assert(matching(FirstVT, SecondVT) && "Types must match");
    }

    /// getIntegerVT - Returns the VTS that represents integers with the given
    /// numbers of bits.
    static VTS getIntegerVT(LLVMContext &Context, unsigned FirstBitWidth,
                            unsigned SecondBitWidth = 0) {
      if (!SecondBitWidth)
        SecondBitWidth = FirstBitWidth;
      return VTS(VT::getIntegerVT(Context, FirstBitWidth),
                 VT::getIntegerVT(Context, SecondBitWidth));
    }

    /// getVectorVT - Returns the VTS that represents vectors with the given
    /// value type and numbers of elements.
    static VTS getVectorVT(LLVMContext &Context, VT ElementVT,
                           unsigned FirstNumElements,
                           unsigned SecondNumElements = 0) {
      if (!SecondNumElements)
        SecondNumElements = FirstNumElements;
      return VTS(VT::getVectorVT(Context, ElementVT, FirstNumElements),
                 VT::getVectorVT(Context, ElementVT, SecondNumElements));
    }

    /// getExpandedIntegerVT - Returns the VTS that represents integers where
    /// the first width is given and the second width is whatever part of the
    /// whole is left over.
    static VTS getExpandedIntegerVT(LLVMContext &Context,
                                    unsigned WholeBitWidth,
                                    unsigned FirstBitWidth = 0) {
      if (!FirstBitWidth)
        FirstBitWidth = WholeBitWidth / 2;
      assert(WholeBitWidth > FirstBitWidth &&
             "Whole is not large enough to split");
      return getIntegerVT(Context, FirstBitWidth,
                          WholeBitWidth - FirstBitWidth);
    }

    /// getHalfSizedIntegerVT - Returns the VTS that represents the WholeVT split into
    /// two integer parts as per EVT::getHalfSizedIntegerVT.
    static VTS getHalfSizedIntegerVT(LLVMContext &Context, VT WholeVT) {
      return VTS::getExpandedIntegerVT(Context, WholeVT.getSizeInBits(),
          WholeVT.getHalfSizedIntegerVT(Context).getSizeInBits());
    }

    /// getExpandedIntegerVT - Returns the VTS that represents integers where
    /// the first num elements is given and the second num elements is whatever
    /// part of the whole is left over.
    static VTS getExpandedVectorVT(LLVMContext &Context, VT ElementVT,
                                   unsigned WholeNumElements,
                                   unsigned FirstNumElements = 0) {
      if (!FirstNumElements)
        FirstNumElements = WholeNumElements / 2;
      assert(WholeNumElements > FirstNumElements &&
             "Whole is not large enough to split");
      return getVectorVT(Context, ElementVT, FirstNumElements,
                         WholeNumElements - FirstNumElements);
    }

    /// getSplitVectorVT - Returns the VTS that represents the WholeVT split
    /// into two vectors with half the elements.
    static VTS getSplitVectorVT(LLVMContext &Context, VT WholeVT) {
      return VTS::getExpandedVectorVT(Context, WholeVT.getVectorElementType(),
                                      WholeVT.getVectorNumElements());
    }

    /// getExpandedVT - Returns the VTS that represents the WholeVT split into
    /// FirstPartVT, and whatever is left over.
    static VTS getExpandedVT(LLVMContext &Context,
                             VT WholeVT, VT FirstVT) {
      assert(matching(WholeVT, FirstVT) && "Types must match");
      // Fast path
      if (WholeVT.getSizeInBits() == FirstVT.getSizeInBits() * 2)
        return FirstVT;
      if (WholeVT.isScalarInteger())
        return getExpandedIntegerVT(Context, WholeVT.getSizeInBits(),
                                    FirstVT.getSizeInBits());
      return getExpandedVectorVT(Context, WholeVT.getVectorElementType(),
                                 WholeVT.getVectorNumElements(),
                                 FirstVT.getVectorNumElements());
    }

    /// getUniqueParts - Returns all of the unique parts.
    ArrayRef<VT> getUniqueParts() const {
      return makeArrayRef(VTs, 2 - isSingle());
    }

    /// getLo - Returns the low part.
    VT getLo() const {
      return VTs[0];
    }

    /// getHi - Returns an high part.
    VT getHi() const {
      return VTs[1];
    }

    /// changeEndianness - Swap the order of the parts.
    void changeEndianness() {
      std::swap(VTs[0], VTs[1]);
    }

    /// getPart - Returns an indexed part.  Note that there is no maximum index
    /// because the types repeat forever.
    VT getPart(unsigned Part) const {
      return VTs[Part % 2];
    }

    /// changeVectorElementTypesToInteger - Return a vector with the same number
    /// of elements as this vector, but with the element type converted to an
    /// integer type with the same bitwidth.
    VTS changeVectorElementTypesToInteger() const {
      return VTS(getLo().changeVectorElementTypeToInteger(),
                 getHi().changeVectorElementTypeToInteger());
    }

    /// changeTypesToInteger - Return the type converted to an equivalently
    /// sized integer or vector with integer element type. Similar to
    /// changeVectorElementTypeToInteger, but also handles scalars.
    VTS changeTypesToInteger() const {
      return VTS(getLo().changeTypeToInteger(), getHi().changeTypeToInteger());
    }

    /// changeTypesToScalarInteger - Return the type converted to an
    /// equivalently sized integer. Similar to changeTypeToInteger, but vectors
    /// are converted to a scalar integer.
    VTS changeTypesToScalarInteger(LLVMContext &Context) const {
      return VTS(getLo().changeTypeToScalarInteger(Context),
                 getHi().changeTypeToScalarInteger(Context));
    }

    /// isSingle - Return if this represents one type repeated.
    bool isSingle() const {
      return getLo() == getHi();
    }

    VT getSingle() const {
      assert(isSingle() && "not single");
      return getLo();
    }

    /// isFloatingPoint - Return true if this is a FP, or a vector FP type.
    bool isFloatingPoint() const {
      return getLo().isFloatingPoint();
    }

    /// isInteger - Return true if this is an integer, or a vector integer type.
    bool isInteger() const {
      return getLo().isInteger();
    }

    /// isScalarInteger - Return true if this is an integer, but not a vector.
    bool isScalarInteger() const {
      return getLo().isScalarInteger();
    }

    /// isVector - Return true if this is a vector value type.
    bool isVector() const {
      return getLo().isVector();
    }

    /// isByteSized - Return true if the bit size is a multiple of 8.
    bool isByteSized() const {
      return getLo().isByteSized() && getHi().isByteSized();
    }

    /// isRound - Return true if the size is a power-of-two number of bytes.
    bool isRound() const {
      return getLo().isRound() && getHi().isRound();
    }

    bool operator==(VTS OtherVTs) const {
      return getLo() == OtherVTs.getLo() &&
             getHi() == OtherVTs.getHi();
    }
    bool operator!=(VTS OtherVTs) const {
      return !(*this == OtherVTs);
    }

    /// getPartSimpleVT - Return the MVT of the given part.
    MVT getPartSimpleVT(unsigned Part) const {
      return getPart(Part).getSimpleVT();
    }

    /// getPartScalarType - If the given part is a vector type, return its
    /// element type, otherwise return that part.
    VT getPartScalarType(unsigned Part) const {
      return getPart(Part).getScalarType();
    }

    /// getVectorElementType - Given one or two repeated vector types, return
    /// the common element type.
    VT getVectorElementType() const {
      return getLo().getVectorElementType();
    }

    /// getLoVectorNumElements - Given one or two repeated vector types, return
    /// the number of elements in the given part.
    unsigned getLoVectorNumElements() const {
      return getLo().getVectorNumElements();
    }

    /// getHiVectorNumElements - Given one or two repeated vector types, return
    /// the number of elements in the given part.
    unsigned getHiVectorNumElements() const {
      return getHi().getVectorNumElements();
    }

    /// getVectorPartNumElements - Given one or two repeated vector types,
    /// return the number of elements in the given part.
    unsigned getVectorPartNumElements(unsigned Part) const {
      return getPart(Part).getVectorNumElements();
    }

    /// getLoSizeInBits - Return the size of the low part in bits.
    unsigned getLoSizeInBits() const {
      return getLo().getSizeInBits();
    }

    /// getHiSizeInBits - Return the size of the high part in bits.
    unsigned getHiSizeInBits() const {
      return getHi().getSizeInBits();
    }

    /// getPartSizeInBits - Return the size of the specified part in bits.
    unsigned getPartSizeInBits(unsigned Part) const {
      return getPart(Part).getSizeInBits();
    }

    /// getPartScalarSizeInBits - Return the size of the specified part's scalar
    /// type.
    unsigned getPartScalarSizeInBits(unsigned Part) const {
      return getPart(Part).getScalarSizeInBits();
    }

    /// getLoStoreSize - Return the number of bytes overwritten by a store of
    /// the low part.
    unsigned getLoStoreSize() const {
      return getLo().getStoreSize();
    }

    /// getHiStoreSize - Return the number of bytes overwritten by a store of
    /// the high part.
    unsigned getHiStoreSize() const {
      return getHi().getStoreSize();
    }

    /// getPartStoreSize - Return the number of bytes overwritten by a store of
    /// the specified part.
    unsigned getPartStoreSize(unsigned Part) const {
      return getPart(Part).getStoreSize();
    }

    /// getPartStoreSizeInBits - Return the number of bits overwritten by a
    /// store of the specified part.
    unsigned getPartStoreSizeInBits(unsigned Part) const {
      return getPart(Part).getStoreSizeInBits();
    }

    /// getVectorPartsNumElements - Return the number of elements of the
    /// specified range of parts.
    unsigned getVectorPartsNumElements(unsigned NumParts = 2,
                                       unsigned FirstPart = 0) const {
      return getPartsSizeGeneric(NumParts, getVectorPartNumElements(FirstPart),
                                 getVectorPartNumElements(FirstPart + 1));
    }

    /// getPartsSizeInBits - Return the size of the specified range of parts
    /// in bits.
    unsigned getPartsSizeInBits(unsigned NumParts = 2,
                                unsigned FirstPart = 0) const {
      return getPartsSizeGeneric(NumParts, getPartSizeInBits(FirstPart),
                                 getPartSizeInBits(FirstPart + 1));
    }

    /// getPartsScalarSizeInBits - Return the size of the scalar type of the
    /// specified range of parts.
    unsigned getPartsScalarSizeInBits(unsigned NumParts = 2,
                                      unsigned FirstPart = 0) const {
      return getPartsSizeGeneric(NumParts, getPartScalarSizeInBits(FirstPart),
                                 getPartScalarSizeInBits(FirstPart + 1));
    }

    /// getPartsStoreSize - Return the number of bytes overwritten by
    /// individual stores (each part is padded independently) of the specified
    /// range of parts.
    unsigned getPartsStoreSize(unsigned NumParts = 2,
                               unsigned FirstPart = 0) const {
      return getPartsSizeGeneric(NumParts, getPartStoreSize(FirstPart),
                                 getPartStoreSize(FirstPart + 1));
    }

    /// getPartsStoreSizeInBits - Return the number of bits overwritten by
    /// individual stores (each part is padded independently) of the specified
    /// range of parts.
    unsigned getPartsStoreSizeInBits(unsigned NumParts = 2,
                                     unsigned FirstPart = 0) const {
      return getPartsSizeGeneric(NumParts, getPartStoreSizeInBits(FirstPart),
                                 getPartStoreSizeInBits(FirstPart + 1));
    }

    /// getPartsVT - Return a VT covering NumParts parts starting with FirstPart.
    VT getPartsVT(LLVMContext &Context, unsigned NumParts = 2,
                  unsigned FirstPart = 0) const {
      if (isScalarInteger())
        return VT::getIntegerVT(Context,
                                getPartsSizeInBits(NumParts, FirstPart));
      return VT::getVectorVT(Context, getVectorElementType(),
                             getVectorPartsNumElements(NumParts, FirstPart));
    }

    /// getNumPartsFor - Return the number of parts required to hold the given
    /// number of bits.
    unsigned getNumPartsFor(unsigned BitWidth) const {
      return getNumPartsForGeneric(BitWidth, getLoSizeInBits(),
                                   getHiSizeInBits());
    }

    /// getNumPartsForVT - Return the number of parts required to hold the given
    /// value type's bits.
    unsigned getNumPartsForVT(VT WholeVT) const {
      assert(matching(getLo(), WholeVT) && "Types must match");
      return getNumPartsFor(WholeVT.getSizeInBits());
    }

  private:
#ifndef NDEBUG
    /// matching - Checks the invariant of this class that all types in the
    /// sequence are related.  This means they are one of:
    /// * both identical
    /// * both scalar integers with different sizes
    /// * both vectors with the same scalar type and differing numbers of
    ///   elements
    static bool matching(VT VT1, VT VT2) {
      if (VT1.isScalarInteger())
        return VT2.isScalarInteger();
      if (VT1.isVector())
        return VT1.getVectorElementType() == VT2.getVectorElementType();
      return VT1 == VT2;
    }
#endif
    static unsigned getPartsSizeGeneric(unsigned NumParts, unsigned FirstSize,
                                        unsigned SecondSize) {
      return NumParts / 2 * (FirstSize + SecondSize) + NumParts % 2 * FirstSize;
    }
    static unsigned getNumPartsForGeneric(unsigned TotalSize,
                                          unsigned FirstSize,
                                          unsigned SecondSize) {
      // By doing --TotalSize, we ensure that we are always 1-2 parts short,
      // which is why we add 1 below, instead of having to handle cases 0-2.
      --TotalSize;
      if (LLVM_LIKELY(FirstSize == SecondSize))
        return (TotalSize + FirstSize) / FirstSize; // Fast path
      return TotalSize / (FirstSize + SecondSize) * 2 + 1 +
        (TotalSize % (FirstSize + SecondSize) >= FirstSize);
    }
  };

  // TODO: replace all uses of this class with VTS
  /// MVTPair - This represents a pair of Machine Value Types.
  class MVTPair {
    MVT VTs[2];
  public:
    MVTPair() {}
    /*implicit*/ MVTPair(MVT VT) : VTs{VT, VT} {}
    MVTPair(MVT First, MVT Second) : VTs{First, Second} {
      assert(First.isInteger() == Second.isInteger() &&
             First.isVector() == Second.isVector());
    }
    static MVTPair expand(MVT LargeVT, MVT FirstVT) {
      assert(LargeVT.isInteger() && FirstVT.isInteger() &&
             LargeVT.bitsGT(FirstVT) && "Only implemented for integers");
      return MVTPair(FirstVT, MVT::getIntegerVT(LargeVT.getSizeInBits() -
                                                FirstVT.getSizeInBits()));
    }

    MVT getFirst() const {
      return VTs[0];
    }
    MVT getSecond() const {
      return VTs[1];
    }
    MVT getLargest() const {
      return getFirst().bitsGE(getSecond()) ? getFirst() : getSecond();
    }
    MVT getSmallest() const {
      return getFirst().bitsLT(getSecond()) ? getFirst() : getSecond();
    }
    /// getPart - Returns the part type for a specified index.
    MVT getPart(unsigned Idx) const {
      return VTs[Idx & 1];
    }
    bool areSame() const {
      return getFirst() == getSecond();
    }
    operator MVT() const {
      assert(areSame() && "Unhandled MVTPair containing different MVT's");
      return getFirst();
    }
    operator EVT() const {
      return operator MVT();
    }
    MVTPair &operator=(MVT VT) {
      VTs[0] = VTs[1] = VT;
      return *this;
    }
    bool operator==(MVTPair VT) const {
      return getFirst() == VT.getFirst() && getSecond() == VT.getSecond();
    }
    bool operator!=(MVTPair VT) const {
      return !(*this == VT);
    }

    bool isFloatingPoint() const {
      return getFirst().isFloatingPoint();
    }
    bool isInteger() const {
      return getFirst().isInteger();
    }
    bool isVector() const {
      return getFirst().isVector();
    }

    unsigned getFirstSizeInBits() const {
      return getFirst().getSizeInBits();
    }
    unsigned getSecondSizeInBits() const {
      return getSecond().getSizeInBits();
    }
    unsigned getPartSizeInBits(unsigned Idx) const {
      return getPart(Idx).getSizeInBits();
    }
    /// Returns the total size of the pair in bits.
    unsigned getTotalSizeInBits() const {
      return getFirstSizeInBits() + getSecondSizeInBits();
    } 
    /// Returns the number of parts needed for the specified number of bits.
    unsigned getNumPartsFor(unsigned BitWidth) const {
      // Fast path
      if (LLVM_LIKELY(getFirstSizeInBits() == getSecondSizeInBits()))
        return (BitWidth + getFirstSizeInBits() - 1) / getFirstSizeInBits();
      return (BitWidth - 1) / getTotalSizeInBits() * 2 + 1 +
        ((BitWidth - 1) % getTotalSizeInBits() >= getFirstSizeInBits());
    }
    unsigned getNumPartsFor(EVT VT) const {
      return getNumPartsFor(VT.getSizeInBits());
    }

    /// Returns the size in bits of the specified number of parts, starting from
    /// the specified first part.
    unsigned getPartsSizeInBits(unsigned NumParts,
                                unsigned FirstPart = 0) const {
      return (NumParts >> 1) * getTotalSizeInBits() +
        (NumParts & 1) * getPartSizeInBits(FirstPart);
    }

    /// getPartsStoreSize - Return the number of bytes overwritten by a store
    /// of the specified value type.
    unsigned getPartsStoreSize(unsigned NumParts,
                               unsigned FirstPart = 0) const {
      return (getPartsSizeInBits(NumParts, FirstPart) + 7) / 8;
    }

    /// getPartsStoreSizeInBits - Return the number of bits overwritten by a
    /// store of the specified value type.
    unsigned getPartsStoreSizeInBits(unsigned NumParts,
                                     unsigned FirstPart = 0) const {
      return getPartsStoreSize(NumParts, FirstPart) * 8;
    }

  };

} // End llvm namespace

#endif
