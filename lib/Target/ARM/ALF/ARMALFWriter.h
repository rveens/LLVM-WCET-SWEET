#ifndef LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H
#define LLVM_LIB_TARGET_ARM_ALF_ARMALFWRITER_H

#include "llvm/ALF/ALFWriter.h"
#include "llvm/ALF/MCInstBB.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"

using namespace llvm;
using namespace alf;

namespace llvm {

	class ARMALFWriter : public ALFWriter {
		const MCInstrInfo *MII;
	 	const MCRegisterInfo *MRI;
		public:
			ARMALFWriter() : ARMALFWriter(nullptr, nullptr)
			{
			}

			ARMALFWriter(const MCInstrInfo *_MII, const MCRegisterInfo *_MRI)
				: MII(_MII), MRI(_MRI), ALFWriter("arm.alf")
			{
				regDefALF(*b); // TableGen
				initFrames();
			}
			virtual ~ARMALFWriter() { }

			// Table'gen'd
			virtual void printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label) override;
			virtual void regDefALF(ALFBuilder &b) override;

			// other
			virtual void HigherMCInstToMachineInstr(shared_ptr<MCInstBB> bb, MachineBasicBlock *mbb, MCInst &mc) override;
		private:
			virtual void initFrames() override;
			void extraFrames(const MachineConstantPool *MCP) override;
			virtual unsigned computeBBcycles(MachineBasicBlock &mbb) override;
			virtual bool shouldSetCondFlags(const MachineInstr &MI) override;
	};
	/* char ARMALFWriter::ID = 0; */
}

#endif
