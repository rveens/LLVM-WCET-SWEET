#include "DummyMCStreamer.h"

void DummyMCStreamer::EmitInstruction(const MCInst &Inst, const MCSubtargetInfo &STI)
{
	static unsigned int instrCounter = 0;

	LabelledInst lInst;

	// if we have more than one label, then print an error
	if (lastLabels.size() > 1) {
		errs() << "Found more than one label, near: " << lastLabels[0] <<  "\n";
	}

	/* Store instruction */
	lInst.inst = Inst;
	// if lastLabels is empty, use an empty string, else the first element.
	lInst.label = (lastLabels.empty() ? "" : lastLabels[0]);
	lInst.origIndex = instrCounter++;

	/* Clear last seen labels */ 
	lastLabels.clear();

	/* add linst to the list */
	this->Insts.push_back(lInst);
}

void DummyMCStreamer::EmitLabel(MCSymbol *Symbol)
{
	lastLabels.push_back(Symbol->getName());
}

void DummyMCStreamer::EmitBytes(StringRef Data)
{
	char bytes[4]; // temporary store to convert from BigEnd -> LittleEnd
	if (Data.size() == 4) {
		int i = 3;
		for (auto I = Data.bytes_begin(); I != Data.bytes_end(); I++, i--) {
			dbgs() << "[i]: " << std::to_string(*I);
			bytes[i] = *I;
		}
		dbgs() << "He ik vond iets: " <<  *((unsigned int*)bytes) << "\n";
	}
}

bool DummyMCStreamer::EmitSymbolAttribute(MCSymbol *Symbol, MCSymbolAttr Attribute)
{
	return true;
}

void DummyMCStreamer::EmitCommonSymbol(MCSymbol *Symbol, uint64_t Size, unsigned ByteAlignment)
{
	/* do nothing */
}

void DummyMCStreamer::EmitZerofill(MCSection *Section, MCSymbol *Symbol, uint64_t Size, unsigned ByteAlignment)
{
	/* do nothing */
}

