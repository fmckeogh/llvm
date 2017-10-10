//===- OMFObjectFile.h - OMF object file implementation ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the OMFObjectFile class, which implements the ObjectFile
// interface for OMF files.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_OBJECT_OMFOBJECTFILE_H
#define LLVM_OBJECT_OMFOBJECTFILE_H

#include "llvm/Object/OMF.h"

namespace llvm {
namespace object {

class OMFObjectFile : public ObjectFile {
public:
  OMFObjectFile(MemoryBufferRef Object, Error &EC);

  static inline bool classof(const Binary *v) { return v->isOMF(); }

protected:
  // Overrides from SymbolRef.
  void moveSymbolNext(DataRefImpl &Symb) const override;
  uint32_t getSymbolFlags(DataRefImpl Symb) const override;
  basic_symbol_iterator symbol_begin() const override;
  basic_symbol_iterator symbol_end() const override;
  Expected<StringRef> getSymbolName(DataRefImpl Symb) const override;
  Expected<uint64_t> getSymbolAddress(DataRefImpl Symb) const override;
  uint64_t getSymbolValueImpl(DataRefImpl Symb) const override;
  uint32_t getSymbolAlignment(DataRefImpl Symb) const override;
  uint64_t getCommonSymbolSizeImpl(DataRefImpl Symb) const override;
  Expected<SymbolRef::Type> getSymbolType(DataRefImpl Symb) const override;
  Expected<section_iterator>
  getSymbolSection(DataRefImpl Symb) const override;

  // Overrides from SectionRef.
  void moveSectionNext(DataRefImpl &Sec) const override;
  std::error_code getSectionName(DataRefImpl Sec,
                                 StringRef &Res) const override;
  uint64_t getSectionAddress(DataRefImpl Sec) const override;
  uint64_t getSectionIndex(DataRefImpl Sec) const override;
  uint64_t getSectionSize(DataRefImpl Sec) const override;
  std::error_code getSectionContents(DataRefImpl Sec,
                                     StringRef &Res) const override;
  uint64_t getSectionAlignment(DataRefImpl Sec) const override;
  bool isSectionCompressed(DataRefImpl Sec) const override;
  bool isSectionText(DataRefImpl Sec) const override;
  bool isSectionData(DataRefImpl Sec) const override;
  bool isSectionBSS(DataRefImpl Sec) const override;
  bool isSectionVirtual(DataRefImpl Sec) const override;
  section_iterator section_begin() const override;
  section_iterator section_end() const override;
  relocation_iterator section_rel_begin(DataRefImpl Sec) const override;
  relocation_iterator section_rel_end(DataRefImpl Sec) const override;

  // Same as above for RelocationRef.
  void moveRelocationNext(DataRefImpl &Rel) const override;
  uint64_t getRelocationOffset(DataRefImpl Rel) const override;
  symbol_iterator getRelocationSymbol(DataRefImpl Rel) const override;
  uint64_t getRelocationType(DataRefImpl Rel) const override;
  void getRelocationTypeName(DataRefImpl Rel,
                             SmallVectorImpl<char> &Result) const override;

public:
  uint8_t getBytesInAddress() const override;
  StringRef getFileFormatName() const override;
  /* Triple::ArchType */ unsigned getArch() const override;
  StringRef getProcessor() const;
  StringRef getModuleName() const;
  uint64_t getMAUBits() const;
  uint64_t getAddrMAUs() const;
  SubtargetFeatures getFeatures() const override;
  bool isRelocatableObject() const override;
  StringRef getSectionType(const SectionRef &Sec) const;

private:
  Error parse();

  std::string FileFormatName = "OMF-";
  StringRef ModuleName;
  uint64_t MAUBits = 8, AddrMAUs = 4;
  support::endianness Endianness = support::big;
  std::vector<OMFContext> Contexts;
  std::vector<OMFSection> Sections;
  std::vector<OMFSymbol> Symbols;
};

} // end namespace object
} // end namespace llvm

#endif // LLVM_OBJECT_OMFOBJECTFILE_H
