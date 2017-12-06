add_llvm_library(LLVMALF
  ALFWriter.cpp
  ALFBuilder.cpp
  ALFContext.cpp
  SExpr.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/ALF
  )
