#ifndef LIB_TARGET_CLP_CLPWCET_OUTPUTFILEDOT_H
#define LIB_TARGET_CLP_CLPWCET_OUTPUTFILEDOT_H

#include <memory>
#include <string>

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Debug.h"

#include "OutputFileInterface.h"
#include "MCInstBB.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCCodeEmitter.h"

using namespace std;

class OutputFileDOT : public OutputFileInterface
{
	MCInstPrinter &instPrinter;
	MCSubtargetInfo const &STI;

public:
	OutputFileDOT(MCInstPrinter &_instPrinter, MCSubtargetInfo const &_STI);
	virtual ~OutputFileDOT();

	void output(std::list<shared_ptr<MCInstBB>> bb, std::list<shared_ptr<MCInstBB>> bblist_unreachable);

private:
	void make_label(raw_fd_ostream &File, std::shared_ptr<MCInstBB> bb, string color);
	void make_arrow(raw_fd_ostream &File, std::shared_ptr<MCInstBB> bb_from, std::shared_ptr<MCInstBB> bb_to, string label);

	bool is_illegal_instruction_pair(MCInstBB::iterator I, std::shared_ptr<MCInstBB> bb);
	bool has_dataconflict_delay(MCInstBB::iterator I, std::shared_ptr<MCInstBB> bb);
	bool has_delay_outside_BasicBlock(MCInstBB::iterator I, std::shared_ptr<MCInstBB> bb);

	unsigned int CLPOpCodeToDelay(LabelledInst &linst);
	string GetOutputRegister(LabelledInst &linst);
	vector<string> GetInputRegisters(LabelledInst &linst);


	inline int startswith(string bigstring, string smallstring)
	{
		return bigstring.compare(0, smallstring.length(), smallstring);
	}
};

#endif
