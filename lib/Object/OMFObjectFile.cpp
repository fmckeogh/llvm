
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

#include "llvm/ADT/Triple.h"
#include "llvm/Object/OMF.h"
#include <algorithm>
#include <numeric>

using namespace llvm;
using namespace object;

Expected<std::unique_ptr<OMFObjectFile>>
ObjectFile::createOMFObjectFile(MemoryBufferRef Object) {
  Error Err = Error::success();
  auto ObjectFile = make_unique<OMFObjectFile>(std::move(Object), Err);
  if (Err)
    return std::move(Err);
  return std::move(ObjectFile);
}

static Optional<StringRef> trySafe(StringRef &Data, uint64_t Size) {
  if (Data.size() >= Size) {
    StringRef Result = Data.take_front(Size);
    Data = Data.drop_front(Size);
    return Result;
  }
  return None;
}

static Expected<StringRef> readSafe(StringRef &Data, uint64_t Size) {
  if (auto Result = trySafe(Data, Size))
    return *Result;
  return make_error<GenericBinaryError>("Unexpected end of part");
}

static Optional<uint8_t> tryByte(StringRef &Data) {
  if (auto Byte = trySafe(Data, 1))
    return Byte->front();
  return None;
}

static Expected<uint8_t> readByte(StringRef &Data) {
  auto Byte = readSafe(Data, 1);
  if (Byte)
    return Byte->front();
  return Byte.takeError();
}

static bool consumeByte(StringRef &Data, char Byte) {
  return Data.consume_front(StringRef(&Byte, 1));
}

static Expected<uint64_t> readBytes(StringRef &Data, uint8_t Size) {
  assert(Size <= 8 && "Can only return up to 64 bits");
  auto Bytes = readSafe(Data, Size);
  if (!Bytes)
    return Bytes.takeError();
  uint64_t Result = 0;
  for (auto Byte : Bytes->bytes())
    Result = Result << 8 | Byte;
  return Result;
}

static bool hasNumber(StringRef Data) {
  auto Type = tryByte(Data);
  return Type && *Type < 0x88;
}

static Expected<uint64_t> readNumber(StringRef &Data) {
  auto Type = readByte(Data);
  if (!Type)
    return Type.takeError();
  if (*Type < 0x80)
    return *Type;
  if (*Type < 0x88)
    return readBytes(Data, *Type & 7);
  return make_error<GenericBinaryError>("Unknown number type 0x" +
                                        Twine::utohexstr(*Type));
}

static Expected<Optional<uint64_t>> tryNumber(StringRef &Data) {
  if (hasNumber(Data))
    return readNumber(Data);
  return None;
}

static bool consumeNumber(StringRef &Data, uint64_t Value) {
  StringRef Temp = Data;
  auto Number = readNumber(Temp);
  if (Number && *Number == Value) {
    Data = Temp;
    return true;
  }
  consumeError(Number.takeError());
  return false;
}

static bool hasString(StringRef Data) {
  auto Type = tryByte(Data);
  return Type && (*Type < 0x80 || *Type == OMF_EL1 || *Type == OMF_EL2);
}

static Expected<StringRef> readString(StringRef &Data) {
  auto Size = readBytes(Data, 1);
  if (Size) {
    if (*Size == OMF_EL1)
      Size = readBytes(Data, 1);
    else if (*Size == OMF_EL2)
      Size = readBytes(Data, 2);
    else if (*Size >= 0x80)
      return make_error<GenericBinaryError>("Unknown string type 0x" +
                                            Twine::utohexstr(*Size));
  }
  if (!Size)
    return Size.takeError();
  return readSafe(Data, *Size);
}

static Expected<Optional<StringRef>> tryString(StringRef &Data) {
  if (hasString(Data))
    return readString(Data);
  return None;
}

static Expected<StringRef> readExpression(StringRef &Data) {
  unsigned Size;
  for (Size = 0; Size < Data.size(); ++Size) {
    uint8_t Byte = Data[Size];
    if (Byte >= OMF_FIRST_RECORD)
      break;
    if (Byte >= 0x80 && Byte < 0x88)
      Size += Byte & 7;
  }
  StringRef Expr = Data.take_front(Size);
  Data = Data.drop_front(Expr.size());
  return Expr;
}

static Expected<uint64_t> lookup(DenseMap<uint64_t, uint64_t> &Map,
                                 Expected<uint64_t> Index, StringRef Type) {
  if (!Index)
    return Index.takeError();
  auto I = Map.find(*Index);
  if (I == Map.end())
    return make_error<GenericBinaryError>("Unknown " + Type + " index " +
                                          Twine(*Index));
  return I->second;
}

Error OMFObjectFile::parse() {
  StringRef Data = getData();
  std::string *CurrentSectionData = nullptr;
  DenseMap<uint64_t, uint64_t> ContextMap, SectionMap, SymbolMap, ReferenceMap;
  DenseMap<uint64_t, std::string> SectionData;
  while (!Data.empty()) {
    switch (uint8_t RecordHeader = cantFail(readByte(Data))) {
    case OMF_RECORD_MB: {
      auto String = readString(Data);
      if (String) {
        FileFormatName += *String;
        String = readString(Data);
      }
      if (!String)
        return String.takeError();
      ModuleName = *String;
      break;
    }
    case OMF_RECORD_ME: {
      if (!Data.empty())
        return make_error<GenericBinaryError>("Unexpected trailing data");
      break;
    }
    case OMF_RECORD_AS: {
      auto Letter = readByte(Data);
      if (!Letter)
        return Letter.takeError();
      switch (*Letter) {
      case OMF_I: {
        auto Symbol = lookup(SymbolMap, readNumber(Data), "symbol");
        if (!Symbol)
          return Symbol.takeError();
        if (hasNumber(Data)) {
          auto Address = readNumber(Data);
          if (!Address)
            return Address.takeError();
          Symbols[*Symbol].Address = *Address;
        } else {
          if (!consumeByte(Data, OMF_R))
            return make_error<GenericBinaryError>("Could not parse symbol " +
                                                  Symbols[*Symbol].Name +
                                                  "'s value");
          auto Section = readNumber(Data);
          if (!Section)
            return Section.takeError();
          consumeByte(Data, OMF_COMMA);
          auto Address = readNumber(Data);
          if (!Address)
            return Address.takeError();
          consumeByte(Data, OMF_COMMA);
          if (!consumeByte(Data, OMF_ADD))
            return make_error<GenericBinaryError>("Could not parse symbol " +
                                                  Symbols[*Symbol].Name +
                                                  "'s value");
          Symbols[*Symbol].Section = *Section;
          Symbols[*Symbol].Address = *Address;
        }
        break;
      }
      case OMF_P: {
        auto Section = readNumber(Data);
        if (!Section)
          return Section.takeError();
        if (!consumeByte(Data, OMF_R) || !consumeNumber(Data, *Section))
          return make_error<GenericBinaryError>("Could not parse set pc");
        consumeByte(Data, OMF_COMMA);
        auto Address = readNumber(Data);
        if (!Address)
          return Address.takeError();
        consumeByte(Data, OMF_COMMA);
        if (!consumeByte(Data, OMF_ADD))
          return make_error<GenericBinaryError>("Could not parse set pc");
        SectionData[*Section].resize(*Address);
        break;
      }
      case OMF_S: {
        auto Section = lookup(SectionMap, readNumber(Data), "section");
        if (!Section)
          return Section.takeError();
        auto Size = readNumber(Data);
        if (!Size)
          return Size.takeError();
        Sections[*Section].Size = *Size;
        break;
      }
      case OMF_W: {
        if (auto Err = readNumber(Data).takeError())
          return Err;
        if (auto Err = readExpression(Data).takeError())
          return Err;
        break;
      }
      default:
        return make_error<GenericBinaryError>("Unknown info var 0x" +
                                              Twine::utohexstr(*Letter));
      }
      break;
    }
    case OMF_RECORD_LR: {
      if (!CurrentSectionData)
        return make_error<GenericBinaryError>("No current section");
      while (!Data.empty()) {
        uint8_t Type = Data.front();
        if (Type < 0x80) {
          auto Bits = readNumber(Data);
          if (!Bits)
            return Bits.takeError();
          auto Bytes = readBytes(Data, *Bits / MAUBits);
          if (!Bytes)
            return Bytes.takeError();
          *CurrentSectionData += *Bytes;
        } else if (Type == OMF_LBRACK ||
                   Type == OMF_LBRACE ||
                   Type == OMF_LPAREN) {
          Data = Data.drop_front(Data.find(Type + 1));
          if (!consumeByte(Data, Type + 1))
            return make_error<GenericBinaryError>("Could not parse loaditem");
#if 0
          if (!consumeByte(Data, OMF_LPAREN))
            return make_error<GenericBinaryError>("Could not parse loaditem");
          auto Var = readByte(Data);
          if (!Var)
            return Var.takeError();
          auto Index = readNumber(Data);
          if (!Index)
            return Index.takeError();
          while (true) {
            consumeByte(Data, OMF_COMMA);
            auto Offset = readNumber(Data);
            if (!Offset)
              return Offset.takeError();
            consumeByte(Data, OMF_COMMA);
            auto Oper = readByte(Data);
            if (!Oper)
              return Oper;
            switch (*Oper) {
              case OMF_ADD:
            }
            
            if (auto Err = tryNumber(Data).takeError())
              return Err;
          if (!consumeByte(Data, OMF_AND) ||
              !consumeNumber(Data, MAUBits) || !consumeByte(Data, Type + 1) ||

              !consumeByte(Data, Type) || !consumeByte(Data, *Var) ||
              !consumeNumber(Data, *Index))
            return make_error<GenericBinaryError>("Could not parse loaditem");
          while (consumeByte(Data, OMF_COMMA))
            if (auto Err = tryNumber(Data).takeError())
              return Err;
          if (!consumeByte(Data, OMF_ESC) || !consumeNumber(Data, 6) ||
              !consumeByte(Data, OMF_COMMA) ||
              !consumeNumber(Data, (1ull << MAUBits) - 1) ||
              !consumeByte(Data, OMF_COMMA) || !consumeByte(Data, OMF_AND) ||
              !consumeNumber(Data, MAUBits) || !consumeByte(Data, Type + 1) ||

              !consumeByte(Data, Type) || !consumeByte(Data, *Var) ||
              !consumeNumber(Data, *Index))
            return make_error<GenericBinaryError>("Could not parse loaditem");
          while (consumeByte(Data, OMF_COMMA))
            if (auto Err = tryNumber(Data).takeError())
              return Err;
          if (!consumeByte(Data, OMF_ESC) || !consumeNumber(Data, 6) ||
              !consumeNumber(Data, MAUBits) || !consumeByte(Data, Type + 1))
            return make_error<GenericBinaryError>("Could not parse loaditem");
#endif
          *CurrentSectionData += '\0';
        } else if (Type >= OMF_FIRST_RECORD)
          break;
        else
          return make_error<GenericBinaryError>("Unknown loaditem 0x" +
                                                Twine::utohexstr(Type));
      }
      break;
    }
    case OMF_RECORD_SB: {
      auto Section = readNumber(Data);
      if (!Section)
        return Section.takeError();
      CurrentSectionData = &SectionData[*Section];
      break;
    }
    case OMF_RECORD_ST: {
      auto Index = readNumber(Data);
      if (!Index)
        return Index.takeError();
      StringRef Type = Data.take_while([](uint8_t Byte) {
          return Byte >= OMF_NULL && Byte <= OMF_Z;
        });
      Data = Data.drop_front(Type.size());
      auto Name = tryString(Data);
      if (!Name)
        return Name.takeError();
      auto Parent = tryNumber(Data);
      if (!Parent)
        return Parent.takeError();
      auto Brother = tryNumber(Data);
      if (!Brother)
        return Brother.takeError();
      auto Context = tryNumber(Data);
      if (!Context)
        return Context.takeError();
      OMFSection Section;
      Section.Type = Type;
      Section.Name = Name->getValueOr(StringRef());
      Section.Parent = Parent->getValueOr(0);
      Section.Brother = Brother->getValueOr(0);
      Section.Context = Context->getValueOr(0);
      SectionMap[*Index] = Sections.size();
      Sections.push_back(std::move(Section));
      break;
    }
    case OMF_RECORD_SA: {
      auto Section = lookup(SectionMap, readNumber(Data), "section");
      if (!Section)
        return Section.takeError();
      auto Align = readNumber(Data);
      if (!Align)
        return Align.takeError();
      auto PageSize = tryNumber(Data);
      if (!PageSize)
        return PageSize.takeError();
      Sections[*Section].Alignment = *Align;
      Sections[*Section].PageSize = PageSize->getValueOr(0);
      break;
    }
    case OMF_RECORD_NI:
    case OMF_RECORD_NX: {
      auto Index = readNumber(Data);
      if (!Index)
        return Index.takeError();
      auto Name = readString(Data);
      if (!Name)
        return Name.takeError();
      OMFSymbol Symbol;
      Symbol.Name = *Name;
      if (RecordHeader == OMF_RECORD_NI) {
        Symbol.Flags = BasicSymbolRef::SF_None;
        SymbolMap[*Index] = Symbols.size();
      } else {
        assert(RecordHeader == OMF_RECORD_NX);
        Symbol.Flags = BasicSymbolRef::SF_Undefined;
        ReferenceMap[*Index] = Symbols.size();
      }
      Symbols.push_back(std::move(Symbol));
      break;
    }
    case OMF_RECORD_AD: {
      auto Number = readNumber(Data);
      if (Number) {
        MAUBits = *Number;
        Number = readNumber(Data);
      }
      if (!Number)
        return Number.takeError();
      AddrMAUs = *Number;
      if (consumeByte(Data, OMF_L))
        Endianness = support::little;
      else if (consumeByte(Data, OMF_M))
        Endianness = support::big;
      break;
    }
    case OMF_RECORD_LD: {
      if (!CurrentSectionData)
        return make_error<GenericBinaryError>("No current section");
      auto Bytes = readString(Data);
      if (!Bytes)
        return Bytes.takeError();
      *CurrentSectionData += *Bytes;
      break;
    }
    case OMF_RECORD_NN: {
      auto Type = readNumber(Data);
      if (!Type)
        return Type.takeError();
      auto Name = readString(Data);
      if (!Name)
        return Name.takeError();
      break;
    }
    case OMF_RECORD_AT: {
      auto Expr = readExpression(Data);
      if (!Expr)
        return Expr.takeError();
      break;
    }
    case OMF_RECORD_NC: {
      auto Index = readNumber(Data);
      if (!Index)
        return Index.takeError();
      auto Name = readString(Data);
      if (!Name)
        return Name.takeError();
      if (auto Err = readExpression(Data).takeError())
        return Err;
      ContextMap[*Index] = Contexts.size();
      Contexts.push_back(std::move(*Name));
      break;
    }
    case OMF_RECORD_IR:
    case OMF_RECORD_C1:
    case OMF_RECORD_C2:
    case OMF_RECORD_TY:
    case OMF_RECORD_WX:
    case OMF_RECORD_RE:
    case OMF_RECORD_BB:
    case OMF_RECORD_BE:
    case OMF_RECORD_LT:
    default:
      return make_error<GenericBinaryError>("Unknown record header 0x" +
                                            Twine::utohexstr(RecordHeader));
    }
  }
  for (auto SectionIndex : SectionMap) {
    OMFSection &Section = Sections[SectionIndex.second];
    if (Section.Parent) {
      auto Parent = lookup(SectionMap, Section.Parent, "section");
      if (!Parent)
        return Parent.takeError();
      Section.Parent = *Parent;
    } else {
      Section.Parent = Sections.size();
    }
    if (Section.Brother) {
      auto Brother = lookup(SectionMap, Section.Brother, "section");
      if (!Brother)
        return Brother.takeError();
      Section.Brother = *Brother;
    } else {
      Section.Brother = Sections.size();
    }
    if (Section.Context) {
      auto Context = lookup(ContextMap, Section.Context, "context");
      if (!Context)
        return Context.takeError();
      Section.Context = *Context;
    } else {
      Section.Context = Contexts.size();
    }
    Section.Data = SectionData[SectionIndex.first];
  }
  for (OMFSymbol &Symbol : Symbols)
    if (Symbol.Section) {
      auto Section = lookup(SectionMap, Symbol.Section, "section");
      if (!Section)
        return Section.takeError();
      Symbol.Section = *Section;
    } else {
      Symbol.Section = Sections.size();
    }
  return Error::success();
}

OMFObjectFile::OMFObjectFile(MemoryBufferRef Buffer, Error &Err)
    : ObjectFile(Binary::ID_OMF, Buffer) {
  ErrorAsOutParameter ErrAsOutParam(&Err);
  Err = parse();
}

void OMFObjectFile::moveSymbolNext(DataRefImpl &Symb) const {
  ++Symb.p;
}

basic_symbol_iterator OMFObjectFile::symbol_begin() const {
  DataRefImpl Symb;
  Symb.p = 0;
  return basic_symbol_iterator(BasicSymbolRef(Symb, this));
}

basic_symbol_iterator OMFObjectFile::symbol_end() const {
  DataRefImpl Symb;
  Symb.p = Symbols.size();
  return basic_symbol_iterator(BasicSymbolRef(Symb, this));
}

Expected<StringRef> OMFObjectFile::getSymbolName(DataRefImpl Symb) const {
  return Symbols[Symb.p].Name;
}

uint32_t OMFObjectFile::getSymbolFlags(DataRefImpl Symb) const {
  return Symbols[Symb.p].Flags;
}

Expected<uint64_t> OMFObjectFile::getSymbolAddress(DataRefImpl Symb) const {
  return Symbols[Symb.p].Address;
}

uint64_t OMFObjectFile::getSymbolValueImpl(DataRefImpl Symb) const {
  llvm_unreachable("Unimplemented!");
}

uint32_t OMFObjectFile::getSymbolAlignment(DataRefImpl Symb) const {
  llvm_unreachable("Unimplemented!");
}

uint64_t OMFObjectFile::getCommonSymbolSizeImpl(DataRefImpl Symb) const {
  llvm_unreachable("Unimplemented!");
}

Expected<SymbolRef::Type> OMFObjectFile::getSymbolType(DataRefImpl Symb) const {
  return SymbolRef::ST_Unknown;
}

Expected<section_iterator>
OMFObjectFile::getSymbolSection(DataRefImpl Symb) const {
  DataRefImpl Sec;
  Sec.p = Symbols[Symb.p].Section;
  return section_iterator(SectionRef(Sec, this));
}

section_iterator OMFObjectFile::section_begin() const {
  DataRefImpl Sec;
  Sec.p = 0;
  return section_iterator(SectionRef(Sec, this));
}

section_iterator OMFObjectFile::section_end() const {
  DataRefImpl Sec;
  Sec.p = Sections.size();
  return section_iterator(SectionRef(Sec, this));
}

void OMFObjectFile::moveSectionNext(DataRefImpl &Sec) const {
  ++Sec.p;
}

std::error_code
OMFObjectFile::getSectionName(DataRefImpl Sec, StringRef &Res) const {
  Res = Sections[Sec.p].Name;
  return std::error_code();
}

uint64_t OMFObjectFile::getSectionAddress(DataRefImpl Sec) const {
  return 0;
}

uint64_t OMFObjectFile::getSectionSize(DataRefImpl Sec) const {
  return Sections[Sec.p].Size;
}

std::error_code OMFObjectFile::getSectionContents(DataRefImpl Sec,
                                                  StringRef &Res) const {
  Res = Sections[Sec.p].Data;
  return std::error_code();
}

uint64_t OMFObjectFile::getSectionAlignment(DataRefImpl Sec) const {
  return Sections[Sec.p].Alignment;
}

bool OMFObjectFile::isSectionCompressed(DataRefImpl Sec) const {
  return false;
}

bool OMFObjectFile::isSectionText(DataRefImpl Sec) const {
  return StringSwitch<bool>(Sections[Sec.p].Name)
    .Case("CODE", true)
    .Default(false);
}

bool OMFObjectFile::isSectionData(DataRefImpl Sec) const {
  return StringSwitch<bool>(Sections[Sec.p].Name)
    .Cases("DATA", "STRSECT", "TEXT", true)
    .Default(false);
}

bool OMFObjectFile::isSectionBSS(DataRefImpl Sec) const {
  return StringSwitch<bool>(Sections[Sec.p].Name)
    .Case("BSS", true)
    .Default(false);
}

StringRef OMFObjectFile::getSectionType(const SectionRef &Sec) const {
  return Sections[Sec.getRawDataRefImpl().p].Type;
}

bool OMFObjectFile::isSectionVirtual(DataRefImpl Sec) const {
  return Sections[Sec.p].Data.empty();
}

relocation_iterator OMFObjectFile::section_rel_begin(DataRefImpl Sec) const {
  llvm_unreachable("Unimplemented!");
}

relocation_iterator OMFObjectFile::section_rel_end(DataRefImpl Sec) const {
  llvm_unreachable("Unimplemented!");
}

void OMFObjectFile::moveRelocationNext(DataRefImpl &Rel) const {
  llvm_unreachable("Unimplemented!");
}

uint64_t OMFObjectFile::getRelocationOffset(DataRefImpl Rel) const {
  llvm_unreachable("Unimplemented!");
}

symbol_iterator OMFObjectFile::getRelocationSymbol(DataRefImpl Rel) const {
  llvm_unreachable("Unimplemented!");
}

uint64_t OMFObjectFile::getRelocationType(DataRefImpl Rel) const {
  llvm_unreachable("Unimplemented!");
}

void OMFObjectFile::getRelocationTypeName(DataRefImpl Rel,
                                          SmallVectorImpl<char> &Result) const {
  llvm_unreachable("Unimplemented!");
}

uint8_t OMFObjectFile::getBytesInAddress() const {
  return AddrMAUs * MAUBits / 8;
}

StringRef OMFObjectFile::getFileFormatName() const {
  return FileFormatName;
}

unsigned OMFObjectFile::getArch() const {
  Triple::ArchType Arch = Triple::getArchTypeForLLVMName(
      StringRef(FileFormatName).substr(4).lower());
  if (Arch == Triple::UnknownArch)
    switch (AddrMAUs) {
    case 2: Arch = Triple::z80; break;
    case 3: Arch = Triple::ez80; break;
    }

  return Arch;
}

StringRef OMFObjectFile::getProcessor() const {
  return getFileFormatName().substr(4);
}

StringRef OMFObjectFile::getModuleName() const {
  return ModuleName;
}

uint64_t OMFObjectFile::getMAUBits() const {
  return MAUBits;
}

uint64_t OMFObjectFile::getAddrMAUs() const {
  return AddrMAUs;
}

SubtargetFeatures OMFObjectFile::getFeatures() const {
  llvm_unreachable("Unimplemented!");
}

bool OMFObjectFile::isRelocatableObject() const {
  llvm_unreachable("Unimplemented!");
}
