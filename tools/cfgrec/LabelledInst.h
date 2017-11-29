#ifndef TARGET_CLP_CLPWCET_LABELLEDINST_H
#define TARGET_CLP_CLPWCET_LABELLEDINST_H

#include "llvm/MC/MCStreamer.h"

using namespace std;
using namespace llvm;

struct LabelledInst {
	MCInst inst;
	string label;
	unsigned int origIndex;
	unsigned branchTarget; // the target if this instruction is a branch.

	bool operator==(const LabelledInst& rhs)
	{
		return this->origIndex == rhs.origIndex;
	}
	bool operator!=(const LabelledInst& rhs)
	{
		return this->origIndex != rhs.origIndex;
	}
};

#endif
