//===-- llvm/Target/TargetInstrInfo.h - Instruction Info --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the target machine instruction set to the code generator.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TARGET_TARGETALFWRITER_H
#define LLVM_TARGET_TARGETALFWRITER_H

namespace llvm {

	class TargetALFWriter {
	public:
		TargetALFWriter ();
		virtual ~TargetALFWriter ();

	private:

	};

} // end namespace llvm

#endif // LLVM_TARGET_TARGETALFWRITER_H
