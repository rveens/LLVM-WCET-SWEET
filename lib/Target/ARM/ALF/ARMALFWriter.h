#ifndef LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H
#define LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H

#include "llvm/ALF/ALFWriter.h"

using namespace llvm;
using namespace alf;

/* namespace llvm { */

	class ARMALFWriter : public ALFWriter {
		public:
			ARMALFWriter() : ALFWriter("arm.alf")
			{
				regDefALF(*b); // TableGen
				initFrames();
			}
			virtual ~ARMALFWriter() { }

			// Table'gen'd
			virtual void printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label) override;
			virtual void regDefALF(ALFBuilder &b) override;
		private:
			virtual void initFrames() override;
			void extraFrames(const MachineConstantPool *MCP);
			virtual unsigned computeBBcycles(MachineBasicBlock &mbb) override;
			virtual bool shouldSetCondFlags(const MachineInstr &MI) override;
	};
	/* char ARMALFWriter::ID = 0; */
/* } */

#endif
