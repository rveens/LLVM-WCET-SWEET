//===-- ALFBuilder.cpp - Creating ALF (Artist2 Language for Flow Analysis) modules --------------===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <llvm/ADT/StringExtras.h>
#include "llvm/Support/FileSystem.h"

#include "llvm/ALF/ALFBuilder.h"

using namespace llvm;

namespace alf {

void ALFBuilder::addInit(const Twine& Name, uint64_t Offset, SExpr* InitValue, bool Volatile, bool ReadOnly) {
    SExprList *Ref  = list("ref")->append(identifier(Name))->append(offset(Offset));
    SExprList *Init = list("init")->append(Ref)
                                  ->append(InitValue);
    if(Volatile) Init->append("volatile");
    if(ReadOnly) Init->append("read_only");
    Initializers.push_back(Init);
}

void ALFBuilder::writeToFile(ALFOutput& Output) {
    // Initialize ALF file
    Output.startList("alf");
    Output.startList("macro_defs");
    Output.endList("macro_defs");
    Output.lauDef();
    Output.atom(Config.isLittleEndian() ? "little_endian" : "big_endian");

    Output.startList("exports");
    // Global variable exports
    Output.startList("frefs");
    // Export Global Frames
    for(std::vector<Frame*>::iterator I = GlobalFrames.begin(), E = GlobalFrames.end(); I!=E; ++I) {
        if((*I)->getStorage() == ExportedFrame)
            Output.fref((*I)->getFrameRef(), false);
    }
    Output.endList("frefs");
    // Export Labels
    Output.startList("lrefs");
    for(std::vector<ALFFunction*>::iterator I = Functions.begin(), E = Functions.end(); I!=E; ++I) {
        if((*I)->isExported()) {
            Output.lref((*I)->getLabel());
        }
    }
    Output.endList("lrefs");
    Output.endList("exports");
    Output.startList("imports");
    // Import Global Frames
    Output.startList("frefs");
    for(std::vector<Frame*>::iterator I = GlobalFrames.begin(), E = GlobalFrames.end(); I!=E; ++I) {
        if((*I)->getStorage() == ImportedFrame)
            Output.fref((*I)->getFrameRef(), false);
    }
    Output.endList("frefs");
    // Import Labels
    Output.startList("lrefs");
    for(std::vector<std::string>::iterator I = ImportedLabels.begin(), E = ImportedLabels.end(); I!=E; ++I) {
        Output.lref(*I, false);
    }
    Output.endList("lrefs");
    Output.endList("imports");
    // Global Declarations
    Output.startList("decls");
    for(std::vector<Frame*>::iterator I = GlobalFrames.begin(), E = GlobalFrames.end(); I!=E; ++I) {
        if((*I)->getStorage() != ImportedFrame)
            Output.alloc((*I)->getFrameRef(), (*I)->getBitWidth());
    }
    Output.endList("decls");
    Output.startList("inits");
    for(std::vector<SExpr*>::iterator I = Initializers.begin(), E = Initializers.end(); I!=E; ++I) {
        Output.sexpr(*I, true);
    }
    Output.endList("inits");
    // Define Functions
    Output.startList("funcs");
    for(std::vector<ALFFunction*>::iterator I = Functions.begin(), E = Functions.end(); I!=E; ++I) {
        writeFunction(*I, Output);
    }
    Output.endList("funcs");
    Output.endList("alf");
}

void ALFBuilder::writeFunction(ALFFunction* AF, ALFOutput& Output) {
    Output.newline();
    Output.comment(AF->getComment(),false);
    Output.startList("func");
    Output.labelRef(AF->getLabel());
    // Emit formal parameters
    Output.startList("arg_decls");
    for(std::vector<Frame*>::iterator I = AF->args_begin(), E = AF->args_end(); I!=E; ++I) {
        Output.alloc((*I)->getFrameRef(), (*I)->getBitWidth());
    }
    Output.endList("arg_decls");

    // start function scope
    Output.startList("scope"); // DECLS INITS STMTS

    // declare local variables
    Output.startList("decls");
    for(std::vector<Frame*>::iterator I = AF->locals_begin(), E = AF->locals_end(); I!=E; ++I) {
        Output.alloc((*I)->getFrameRef(), (*I)->getBitWidth());
        Output.comment((*I)->getDescription());
    }
    Output.endList("decls");
    Output.startList("inits");
    Output.endList("inits");
    // print the basic blocks
    Output.startList("stmts");
    for(std::vector<ALFStatementGroup*>::iterator I = AF->groups_begin(), E = AF->groups_end(); I!=E; ++I) {
        writeStatementGroup(AF, Output, *I);
    }
    Output.endList("stmts");
    Output.endList("scope");
    Output.endList("func");
}

void ALFBuilder::writeStatementGroup(ALFFunction* AF, ALFOutput& Output, ALFStatementGroup *Group) {
    Output.newline();
    Output.comment("--------- BASIC BLOCK " + Group->getComment() + " ----------",false);
    Output.incrementIndent();
    unsigned Ix = 0;
    for(std::vector<ALFStatement*>::iterator I = Group->stmts_begin(), E = Group->stmts_end(); I!=E; ++I, ++Ix) {
        if(Ix == 0) {
             Output.labelRef(Group->getLabel());
        }
        Output.newline();
        if((*I)->getComment().size() > 0)
            Output.comment((*I)->getComment(),false);
        if(Ix > 0) {
            Output.labelRef((*I)->getLabel());
        }
        Output.sexpr((*I)->getCode());
    }
    Output.decrementIndent();
}

void ALFBuilder::writeMapFile(const std::string& FileName) {
    std::error_code EC;
    raw_fd_ostream Out(FileName.c_str(), EC, sys::fs::F_None);
    for(std::map<std::string, std::string>::iterator I = SourceCodeMapping.begin(), E = SourceCodeMapping.end();I!=E;++I) {
        Out << I->first << ";" << I->second << "\n";
    }
    Out.close();
}


} // end namespace alf
