#include "ARMALFWriter.h"

#include <string>

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "ARMGenALFWriter.inc"

bool ARMALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

	std::string Filename = "arm.alf";

	std::error_code EC;
	raw_fd_ostream File(Filename, EC, sys::fs::F_Text);

	if (EC)
		return false;

	ALFOutput o(File, 8);
	ALFBuilder b(o);

	b.setBitWidths(32, 32, 32);
	b.setLittleEndian(false);

	regDefALF(b); // TableGen

	auto alffunc = b.addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");


	for (MachineBasicBlock &mbb : MF) {
		unsigned instrCounter = 0;
		auto alfbb = alffunc->addBasicBlock(mbb.getFullName() + std::to_string(instrCounter), mbb.getFullName() + std::to_string(instrCounter));
		/* dbgs() << mbb.getFullName() << "\n\n"; */
		for (MachineInstr &mi : mbb) {
			/* dbgs() << ">>>" << TII->getName(mi.getOpcode()) << "\n"; */
			string labelName = mbb.getFullName() + std::to_string(instrCounter);
			printInstructionALF(mi, *alfbb, alffunc, labelName); // TableGen
			instrCounter++;
		}
		/* dbgs() << "\n"; */
	}
	b.writeToFile(o);
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
