#include "ARM.h"
#include "ARMALFWriter.h"

/* #include "ARMGenALFWriter.inc" */

#include "ALF/ALFBuilder.h"

using namespace llvm;
using namespace alf;

bool ARMALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

	ALFOutput o(dbgs(), 8);
	ALFBuilder b(o);

	b.setBitWidths(32, 32, 32);
	b.setLittleEndian(false);

	b.writeToFile(o);

	for (MachineBasicBlock &mbb : MF) {
		/* dbgs() << mbb.getFullName() << "\n\n"; */
		for (MachineInstr &mi : mbb) {
			/* dbgs() << ">>>" << TII->getName(mi.getOpcode()) << "\n"; */
			/* printInstructionALF(mi, dbgs()); */
		}
		/* dbgs() << "\n"; */
	}
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
