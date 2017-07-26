#include "ARMALFWriter.h"

#include "ARMGenALFWriter.inc"

bool ARMALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

	ALFOutput o(dbgs(), 8);
	ALFBuilder b(o);

	b.setBitWidths(32, 32, 32);
	b.setLittleEndian(false);

	regDefALF(b); // TableGen

	auto alffunc = b.addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");


	for (MachineBasicBlock &mbb : MF) {
		auto alfbb = alffunc->addBasicBlock(mbb.getName(), "test");
		/* dbgs() << mbb.getFullName() << "\n\n"; */
		for (MachineInstr &mi : mbb) {
			/* dbgs() << ">>>" << TII->getName(mi.getOpcode()) << "\n"; */
			printInstructionALF(mi, *alfbb, alffunc); // TableGen
		}
		/* dbgs() << "\n"; */
	}
	b.writeToFile(o);
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
