//===-- OMFDumper.cpp - OMF-specific dumper ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements the OMF-specific dumper for llvm-readobj.
///
//===----------------------------------------------------------------------===//

#include "Error.h"
#include "ObjDumper.h"
#include "llvm-readobj.h"
#include "llvm/Object/OMF.h"
#include "llvm/Support/ScopedPrinter.h"

using namespace llvm;
using namespace object;
using namespace support;

namespace {

class OMFDumper : public ObjDumper {
  const OMFObjectFile *Obj;
public:
  OMFDumper(const OMFObjectFile *Obj, ScopedPrinter &Writer)
      : ObjDumper(Writer), Obj(Obj) {}

  void printFileHeaders() override;
  void printSections() override;
  void printRelocations() override;
  void printSymbols() override;
  void printDynamicSymbols() override;
  void printUnwindInfo() override;
  void printStackMap() const override;
};

} // end namespace

namespace llvm {

std::error_code createOMFDumper(const object::ObjectFile *Obj,
                                ScopedPrinter &Writer,
                                std::unique_ptr<ObjDumper> &Result) {
  if (const OMFObjectFile *OMFObj = dyn_cast<OMFObjectFile>(Obj)) {
    Result.reset(new OMFDumper(OMFObj, Writer));
    return readobj_error::success;
  }
  return readobj_error::unsupported_obj_file_format;
}

} // namespace llvm

void OMFDumper::printFileHeaders() {
  DictScope D(W, "ModuleBegin");
  W.printString("Processor", Obj->getProcessor());
  W.printString("ModuleName", Obj->getModuleName());
  W.printNumber("MAUBits", Obj->getMAUBits());
  W.printNumber("AddrMAUs", Obj->getAddrMAUs());
}

void OMFDumper::printSections() {
  ListScope Sections(W, "Sections");
  for (const SectionRef &Sec : Obj->sections()) {
    StringRef Name;
    error(Sec.getName(Name));

    DictScope Section(W, "Section");
    W.printString("Name", Name);
    W.printBinary("Type", Obj->getSectionType(Sec));
    W.printHex("Address", Sec.getAddress());
    W.printHex("Size", Sec.getSize());
    W.printHex("Alignment", Sec.getAlignment());

    if (opts::SectionData && !Sec.isVirtual()) {
      StringRef Data;
      error(Sec.getContents(Data));
      W.printBinaryBlock("Data", Data);
    }
  }
}

void OMFDumper::printRelocations() {
  llvm_unreachable("Unimplemented!");
}

void OMFDumper::printSymbols() {
  ListScope Symbols(W, "Symbols");

  for (const SymbolRef &Symb : Obj->symbols()) {
    StringRef SecName;
    error(unwrapOrError(Symb.getSection())->getName(SecName));

    DictScope Symbol(W, "Symbol");
    W.printString("Name", unwrapOrError(Symb.getName()));
    W.printString("Section", SecName);
    W.printHex("Address", unwrapOrError(Symb.getAddress()));
  }
}

void OMFDumper::printDynamicSymbols() {
  llvm_unreachable("Unimplemented!");
}

void OMFDumper::printUnwindInfo() {
  llvm_unreachable("Unimplemented!");
}

void OMFDumper::printStackMap() const {
  llvm_unreachable("Unimplemented!");
}
