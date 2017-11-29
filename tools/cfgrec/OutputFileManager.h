#ifndef LIB_TARGET_CLP_CLPWCET_OUTPUTMANAGER_H
#define LIB_TARGET_CLP_CLPWCET_OUTPUTMANAGER_H

#include <memory>
#include "CFGReconstr.h"
#include "OutputFileInterface.h"


class OutputFileManager
{
private: // fields
	std::vector<shared_ptr<OutputFileInterface>> outputs;

public:
	OutputFileManager()
	{
	}
	virtual ~OutputFileManager() { }
	
	void addOutput(shared_ptr<OutputFileInterface> output)
	{
		outputs.push_back(output);
	}

	void doOutputs(std::list<shared_ptr<MCInstBB>> bblist, std::list<shared_ptr<MCInstBB>> bblist_unreachable) 
	{
		for (auto output : outputs)
			output->output(bblist, bblist_unreachable);
	}
};

#endif
