set(LLVM_TARGET_DEFINITIONS Z80.td)

tablegen(LLVM Z80GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM Z80GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM Z80GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM EZ80GenAsmWriter.inc -gen-asm-writer -asmwriternum=1)
tablegen(LLVM Z80GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM Z80GenCallingConv.inc -gen-callingconv)
tablegen(LLVM Z80GenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(Z80CommonTableGen)

set(sources
  Z80AsmPrinter.cpp
  Z80FrameLowering.cpp
  Z80ISelDAGToDAG.cpp
  Z80ISelLowering.cpp
  Z80InstrInfo.cpp
  Z80MCInstLower.cpp
  Z80RegisterInfo.cpp
  Z80Subtarget.cpp
  Z80TargetMachine.cpp
  )

add_llvm_target(Z80CodeGen ${sources})

add_subdirectory(InstPrinter)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
