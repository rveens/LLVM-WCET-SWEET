set(LLVM_LINK_COMPONENTS
  AllTargetsAsmPrinters
  AllTargetsDescs
  AllTargetsDisassemblers
  AllTargetsInfos
  CodeGen
  DebugInfoDWARF
  DebugInfoPDB
  Demangle
  MC
  MCDisassembler
  Object
  Support
  Symbolize
  )

add_llvm_tool(cfgrec
  cfgrec.cpp
  COFFDump.cpp
  ELFDump.cpp
  MachODump.cpp
  WasmDump.cpp
  CFGReconstr.cpp
  DummyMCStreamer.cpp
  OutputFileDOT.cpp
  )

if(HAVE_LIBXAR)
  target_link_libraries(llvm-objdump ${XAR_LIB})
endif()
