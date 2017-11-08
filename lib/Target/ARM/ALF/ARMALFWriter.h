#ifndef LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H
#define LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H

#include "ARM.h"
#include "ALFBuilder.h"

using namespace llvm;
using namespace alf;

namespace {

	class ARMALFWriter : public MachineFunctionPass {
		public:
			static char ID;

			ARMALFWriter() : MachineFunctionPass(ID) {  }

			bool runOnMachineFunction(MachineFunction &MF) override;

			// Table'gen'd
			void printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label);
			void regDefALF(ALFBuilder &b);
		private:
			void extraFrames(ALFBuilder &b);
			void initFrames(ALFBuilder &b);
			bool shouldSetCondFlags(const MachineInstr &MI);
			unsigned computeBBcycles(MachineBasicBlock &mbb);
	};
	char ARMALFWriter::ID = 0;
}

#endif
