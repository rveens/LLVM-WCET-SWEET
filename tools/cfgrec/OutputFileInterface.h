#ifndef LIB_TARGET_CLP_CLPWCET_OUTPUTINTERFACE_H
#define LIB_TARGET_CLP_CLPWCET_OUTPUTINTERFACE_H

#include <memory>
#include <list>

#include "llvm/ALF/MCInstBB.h"

#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCInstrInfo.h"

class OutputFileInterface
{
public:
	OutputFileInterface()
	{
	}
	virtual void output(std::list<shared_ptr<MCInstBB>> bblist, std::list<shared_ptr<MCInstBB>> bblist_unreachable) = 0;
};

#endif
