#ifndef LLVM_ALF_ALFWRITER_H
#define LLVM_ALF_ALFWRITER_H

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/ALF/ALFBuilder.h"
#include "llvm/ALF/MCInstBB.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

using namespace llvm;
using namespace alf;

namespace llvm {

	class ALFWriter : public MachineFunctionPass
	{
	protected:
		shared_ptr<ALFOutput> o;
		shared_ptr<ALFBuilder> b;
		vector<pair<string, unsigned>> BasicBlockCycles;
	public:
		ALFWriter(string Filename, char ID);
		virtual ~ALFWriter();

		virtual bool runOnMachineFunction(MachineFunction &MF) override;

		virtual void initFrames() { };
		virtual void extraFrames(const MachineConstantPool *MCP) { } ;
		virtual unsigned computeBBcycles(MachineBasicBlock &mbb) = 0;
		virtual bool shouldSetCondFlags(const MachineInstr &MI) = 0;

		virtual void HigherMCInstToMachineInstr(shared_ptr<MCInstBB> bb, MachineBasicBlock *mbb, MCInst &mc) { };

		// expected from tablegen
		virtual void printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label) = 0;
		virtual void regDefALF(ALFBuilder &b) = 0;

	private:
		shared_ptr<raw_fd_ostream> File;

		string makeBBALFname(MachineFunction &MF, MachineBasicBlock &mbb);
	};
}

#endif
