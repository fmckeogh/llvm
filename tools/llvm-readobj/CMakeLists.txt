set(LLVM_LINK_COMPONENTS
  DebugInfoCodeView
  Object
  BinaryFormat
  Support
  DebugInfoCodeView
  DebugInfoMSF
  DebugInfoPDB
  )

add_llvm_tool(llvm-readobj
  ARMWinEHPrinter.cpp
  COFFDumper.cpp
  COFFImportDumper.cpp
  ELFDumper.cpp
  Error.cpp
  llvm-readobj.cpp
  MachODumper.cpp
  ObjDumper.cpp
  OMFDumper.cpp
  WasmDumper.cpp
  Win64EHDumper.cpp
  WindowsResourceDumper.cpp
  )

add_llvm_tool_symlink(llvm-readelf llvm-readobj)
