#include "ARM.h"
#include "ARMALFPrinter.h"

#include "ARMGenALFWriter.inc"

using namespace llvm;


bool ARMALFPrinter::runOnMachineFunction(MachineFunction &MF)
{
	const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

	for (MachineBasicBlock &mbb : MF) {
		dbgs() << mbb.getFullName() << "\n\n";
		for (MachineInstr &mi : mbb) {
			/* dbgs() << ">>>" << TII->getName(mi.getOpcode()) << "\n"; */
			printInstructionALF(mi, dbgs());
		}
		dbgs() << "\n";
	}
}

FunctionPass *llvm::createARMALFPrinterPass() {
  return new ARMALFPrinter();
}
