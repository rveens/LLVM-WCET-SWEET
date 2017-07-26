#include "ARMFeatures.h"
#include "MCTargetDesc/ARMAddressingModes.h"
#include "MCTargetDesc/ARMBaseInfo.h"
#include "MCTargetDesc/ARMMCExpr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCAsmParserUtils.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSection.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ARMBuildAttributes.h"
#include "llvm/Support/ARMEHABI.h"
#include "llvm/Support/COFF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetParser.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

enum class ImplicitItModeTy { Always, Never, ARMOnly, ThumbOnly };

static cl::opt<ImplicitItModeTy> ImplicitItMode(
    "arm-implicit-it", cl::init(ImplicitItModeTy::ARMOnly),
    cl::desc("Allow conditional instructions outdside of an IT block"),
    cl::values(clEnumValN(ImplicitItModeTy::Always, "always",
                          "Accept in both ISAs, emit implicit ITs in Thumb"),
               clEnumValN(ImplicitItModeTy::Never, "never",
                          "Warn in ARM, reject in Thumb"),
               clEnumValN(ImplicitItModeTy::ARMOnly, "arm",
                          "Accept in ARM, reject in Thumb"),
               clEnumValN(ImplicitItModeTy::ThumbOnly, "thumb",
                          "Warn in ARM, emit implicit ITs in Thumb")));

class ARMOperand;

enum VectorLaneTy { NoLanes, AllLanes, IndexedLane };

class UnwindContext {
  MCAsmParser &Parser;

  typedef SmallVector<SMLoc, 4> Locs;

  Locs FnStartLocs;
  Locs CantUnwindLocs;
  Locs PersonalityLocs;
  Locs PersonalityIndexLocs;
  Locs HandlerDataLocs;
  int FPReg;

public:
  UnwindContext(MCAsmParser &P) : Parser(P), FPReg(ARM::SP) {}

  bool hasFnStart() const { return !FnStartLocs.empty(); }
  bool cantUnwind() const { return !CantUnwindLocs.empty(); }
  bool hasHandlerData() const { return !HandlerDataLocs.empty(); }
  bool hasPersonality() const {
    return !(PersonalityLocs.empty() && PersonalityIndexLocs.empty());
  }

  void recordFnStart(SMLoc L) { FnStartLocs.push_back(L); }
  void recordCantUnwind(SMLoc L) { CantUnwindLocs.push_back(L); }
  void recordPersonality(SMLoc L) { PersonalityLocs.push_back(L); }
  void recordHandlerData(SMLoc L) { HandlerDataLocs.push_back(L); }
  void recordPersonalityIndex(SMLoc L) { PersonalityIndexLocs.push_back(L); }

  void saveFPReg(int Reg) { FPReg = Reg; }
  int getFPReg() const { return FPReg; }

  void emitFnStartLocNotes() const {
    for (Locs::const_iterator FI = FnStartLocs.begin(), FE = FnStartLocs.end();
         FI != FE; ++FI)
      Parser.Note(*FI, ".fnstart was specified here");
  }
  void emitCantUnwindLocNotes() const {
    for (Locs::const_iterator UI = CantUnwindLocs.begin(),
                              UE = CantUnwindLocs.end(); UI != UE; ++UI)
      Parser.Note(*UI, ".cantunwind was specified here");
  }
  void emitHandlerDataLocNotes() const {
    for (Locs::const_iterator HI = HandlerDataLocs.begin(),
                              HE = HandlerDataLocs.end(); HI != HE; ++HI)
      Parser.Note(*HI, ".handlerdata was specified here");
  }
  void emitPersonalityLocNotes() const {
    for (Locs::const_iterator PI = PersonalityLocs.begin(),
                              PE = PersonalityLocs.end(),
                              PII = PersonalityIndexLocs.begin(),
                              PIE = PersonalityIndexLocs.end();
         PI != PE || PII != PIE;) {
      if (PI != PE && (PII == PIE || PI->getPointer() < PII->getPointer()))
        Parser.Note(*PI++, ".personality was specified here");
      else if (PII != PIE && (PI == PE || PII->getPointer() < PI->getPointer()))
        Parser.Note(*PII++, ".personalityindex was specified here");
      else
        llvm_unreachable(".personality and .personalityindex cannot be "
                         "at the same location");
    }
  }

  void reset() {
    FnStartLocs = Locs();
    CantUnwindLocs = Locs();
    PersonalityLocs = Locs();
    HandlerDataLocs = Locs();
    PersonalityIndexLocs = Locs();
    FPReg = ARM::SP;
  }
};

class ARMAsmParser : public MCTargetAsmParser {
  const MCInstrInfo &MII;
  const MCRegisterInfo *MRI;
  UnwindContext UC;

  ARMTargetStreamer &getTargetStreamer() {
    assert(getParser().getStreamer().getTargetStreamer() &&
           "do not have a target streamer");
    MCTargetStreamer &TS = *getParser().getStreamer().getTargetStreamer();
    return static_cast<ARMTargetStreamer &>(TS);
  }

  // Map of register aliases registers via the .req directive.
  StringMap<unsigned> RegisterReqs;

  bool NextSymbolIsThumb;

  bool useImplicitITThumb() const {
    return ImplicitItMode == ImplicitItModeTy::Always ||
           ImplicitItMode == ImplicitItModeTy::ThumbOnly;
  }

  bool useImplicitITARM() const {
    return ImplicitItMode == ImplicitItModeTy::Always ||
           ImplicitItMode == ImplicitItModeTy::ARMOnly;
  }

  struct {
    ARMCC::CondCodes Cond;    // Condition for IT block.
    unsigned Mask:4;          // Condition mask for instructions.
                              // Starting at first 1 (from lsb).
                              //   '1'  condition as indicated in IT.
                              //   '0'  inverse of condition (else).
                              // Count of instructions in IT block is
                              // 4 - trailingzeroes(mask)
                              // Note that this does not have the same encoding
                              // as in the IT instruction, which also depends
                              // on the low bit of the condition code.

    unsigned CurPosition;     // Current position in parsing of IT
                              // block. In range [0,4], with 0 being the IT
                              // instruction itself. Initialized according to
                              // count of instructions in block.  ~0U if no
                              // active IT block.

    bool IsExplicit;          // true  - The IT instruction was present in the
                              //         input, we should not modify it.
                              // false - The IT instruction was added
                              //         implicitly, we can extend it if that
                              //         would be legal.
  } ITState;

  llvm::SmallVector<MCInst, 4> PendingConditionalInsts;

  void flushPendingInstructions(MCStreamer &Out) override {
    if (!inImplicitITBlock()) {
      assert(PendingConditionalInsts.size() == 0);
      return;
    }

    // Emit the IT instruction
    unsigned Mask = getITMaskEncoding();
    MCInst ITInst;
    ITInst.setOpcode(ARM::t2IT);
    ITInst.addOperand(MCOperand::createImm(ITState.Cond));
    ITInst.addOperand(MCOperand::createImm(Mask));
    Out.EmitInstruction(ITInst, getSTI());

    // Emit the conditonal instructions
    assert(PendingConditionalInsts.size() <= 4);
    for (const MCInst &Inst : PendingConditionalInsts) {
      Out.EmitInstruction(Inst, getSTI());
    }
    PendingConditionalInsts.clear();

    // Clear the IT state
    ITState.Mask = 0;
    ITState.CurPosition = ~0U;
  }

  bool inITBlock() { return ITState.CurPosition != ~0U; }
  bool inExplicitITBlock() { return inITBlock() && ITState.IsExplicit; }
  bool inImplicitITBlock() { return inITBlock() && !ITState.IsExplicit; }
  bool lastInITBlock() {
    return ITState.CurPosition == 4 - countTrailingZeros(ITState.Mask);
  }
  void forwardITPosition() {
    if (!inITBlock()) return;
    // Move to the next instruction in the IT block, if there is one. If not,
    // mark the block as done, except for implicit IT blocks, which we leave
    // open until we find an instruction that can't be added to it.
    unsigned TZ = countTrailingZeros(ITState.Mask);
    if (++ITState.CurPosition == 5 - TZ && ITState.IsExplicit)
      ITState.CurPosition = ~0U; // Done with the IT block after this.
  }

  // Rewind the state of the current IT block, removing the last slot from it.
  void rewindImplicitITPosition() {
    assert(inImplicitITBlock());
    assert(ITState.CurPosition > 1);
    ITState.CurPosition--;
    unsigned TZ = countTrailingZeros(ITState.Mask);
    unsigned NewMask = 0;
    NewMask |= ITState.Mask & (0xC << TZ);
    NewMask |= 0x2 << TZ;
    ITState.Mask = NewMask;
  }

  // Rewind the state of the current IT block, removing the last slot from it.
  // If we were at the first slot, this closes the IT block.
  void discardImplicitITBlock() {
    assert(inImplicitITBlock());
    assert(ITState.CurPosition == 1);
    ITState.CurPosition = ~0U;
    return;
  }

  // Get the encoding of the IT mask, as it will appear in an IT instruction.
  unsigned getITMaskEncoding() {
    assert(inITBlock());
    unsigned Mask = ITState.Mask;
    unsigned TZ = countTrailingZeros(Mask);
    if ((ITState.Cond & 1) == 0) {
      assert(Mask && TZ <= 3 && "illegal IT mask value!");
      Mask ^= (0xE << TZ) & 0xF;
    }
    return Mask;
  }

  // Get the condition code corresponding to the current IT block slot.
  ARMCC::CondCodes currentITCond() {
    unsigned MaskBit;
    if (ITState.CurPosition == 1)
      MaskBit = 1;
    else
      MaskBit = (ITState.Mask >> (5 - ITState.CurPosition)) & 1;

    return MaskBit ? ITState.Cond : ARMCC::getOppositeCondition(ITState.Cond);
  }

  // Invert the condition of the current IT block slot without changing any
  // other slots in the same block.
  void invertCurrentITCondition() {
    if (ITState.CurPosition == 1) {
      ITState.Cond = ARMCC::getOppositeCondition(ITState.Cond);
    } else {
      ITState.Mask ^= 1 << (5 - ITState.CurPosition);
    }
  }

  // Returns true if the current IT block is full (all 4 slots used).
  bool isITBlockFull() {
    return inITBlock() && (ITState.Mask & 1);
  }

  // Extend the current implicit IT block to have one more slot with the given
  // condition code.
  void extendImplicitITBlock(ARMCC::CondCodes Cond) {
    assert(inImplicitITBlock());
    assert(!isITBlockFull());
    assert(Cond == ITState.Cond ||
           Cond == ARMCC::getOppositeCondition(ITState.Cond));
    unsigned TZ = countTrailingZeros(ITState.Mask);
    unsigned NewMask = 0;
    // Keep any existing condition bits.
    NewMask |= ITState.Mask & (0xE << TZ);
    // Insert the new condition bit.
    NewMask |= (Cond == ITState.Cond) << TZ;
    // Move the trailing 1 down one bit.
    NewMask |= 1 << (TZ - 1);
    ITState.Mask = NewMask;
  }

  // Create a new implicit IT block with a dummy condition code.
  void startImplicitITBlock() {
    assert(!inITBlock());
    ITState.Cond = ARMCC::AL;
    ITState.Mask = 8;
    ITState.CurPosition = 1;
    ITState.IsExplicit = false;
    return;
  }

  // Create a new explicit IT block with the given condition and mask. The mask
  // should be in the parsed format, with a 1 implying 't', regardless of the
  // low bit of the condition.
  void startExplicitITBlock(ARMCC::CondCodes Cond, unsigned Mask) {
    assert(!inITBlock());
    ITState.Cond = Cond;
    ITState.Mask = Mask;
    ITState.CurPosition = 0;
    ITState.IsExplicit = true;
    return;
  }

  void Note(SMLoc L, const Twine &Msg, SMRange Range = None) {
    return getParser().Note(L, Msg, Range);
  }
  bool Warning(SMLoc L, const Twine &Msg, SMRange Range = None) {
    return getParser().Warning(L, Msg, Range);
  }
  bool Error(SMLoc L, const Twine &Msg, SMRange Range = None) {
    return getParser().Error(L, Msg, Range);
  }

  bool validatetLDMRegList(const MCInst &Inst, const OperandVector &Operands,
                           unsigned ListNo, bool IsARPop = false);
  bool validatetSTMRegList(const MCInst &Inst, const OperandVector &Operands,
                           unsigned ListNo);

  int tryParseRegister();
  bool tryParseRegisterWithWriteBack(OperandVector &);
  int tryParseShiftRegister(OperandVector &);
  bool parseRegisterList(OperandVector &);
  bool parseMemory(OperandVector &);
  bool parseOperand(OperandVector &, StringRef Mnemonic);
  bool parsePrefix(ARMMCExpr::VariantKind &RefKind);
  bool parseMemRegOffsetShift(ARM_AM::ShiftOpc &ShiftType,
                              unsigned &ShiftAmount);
  bool parseLiteralValues(unsigned Size, SMLoc L);
  bool parseDirectiveThumb(SMLoc L);
  bool parseDirectiveARM(SMLoc L);
  bool parseDirectiveThumbFunc(SMLoc L);
  bool parseDirectiveCode(SMLoc L);
  bool parseDirectiveSyntax(SMLoc L);
  bool parseDirectiveReq(StringRef Name, SMLoc L);
  bool parseDirectiveUnreq(SMLoc L);
  bool parseDirectiveArch(SMLoc L);
  bool parseDirectiveEabiAttr(SMLoc L);
  bool parseDirectiveCPU(SMLoc L);
  bool parseDirectiveFPU(SMLoc L);
  bool parseDirectiveFnStart(SMLoc L);
  bool parseDirectiveFnEnd(SMLoc L);
  bool parseDirectiveCantUnwind(SMLoc L);
  bool parseDirectivePersonality(SMLoc L);
  bool parseDirectiveHandlerData(SMLoc L);
  bool parseDirectiveSetFP(SMLoc L);
  bool parseDirectivePad(SMLoc L);
  bool parseDirectiveRegSave(SMLoc L, bool IsVector);
  bool parseDirectiveInst(SMLoc L, char Suffix = '\0');
  bool parseDirectiveLtorg(SMLoc L);
  bool parseDirectiveEven(SMLoc L);
  bool parseDirectivePersonalityIndex(SMLoc L);
  bool parseDirectiveUnwindRaw(SMLoc L);
  bool parseDirectiveTLSDescSeq(SMLoc L);
  bool parseDirectiveMovSP(SMLoc L);
  bool parseDirectiveObjectArch(SMLoc L);
  bool parseDirectiveArchExtension(SMLoc L);
  bool parseDirectiveAlign(SMLoc L);
  bool parseDirectiveThumbSet(SMLoc L);

  StringRef splitMnemonic(StringRef Mnemonic, unsigned &PredicationCode,
                          bool &CarrySetting, unsigned &ProcessorIMod,
                          StringRef &ITMask);
  void getMnemonicAcceptInfo(StringRef Mnemonic, StringRef FullInst,
                             bool &CanAcceptCarrySet,
                             bool &CanAcceptPredicationCode);

  void tryConvertingToTwoOperandForm(StringRef Mnemonic, bool CarrySetting,
                                     OperandVector &Operands);
  bool isThumb() const {
    // FIXME: Can tablegen auto-generate this?
    return getSTI().getFeatureBits()[ARM::ModeThumb];
  }
  bool isThumbOne() const {
    return isThumb() && !getSTI().getFeatureBits()[ARM::FeatureThumb2];
  }
  bool isThumbTwo() const {
    return isThumb() && getSTI().getFeatureBits()[ARM::FeatureThumb2];
  }
  bool hasThumb() const {
    return getSTI().getFeatureBits()[ARM::HasV4TOps];
  }
  bool hasThumb2() const {
    return getSTI().getFeatureBits()[ARM::FeatureThumb2];
  }
  bool hasV6Ops() const {
    return getSTI().getFeatureBits()[ARM::HasV6Ops];
  }
  bool hasV6T2Ops() const {
    return getSTI().getFeatureBits()[ARM::HasV6T2Ops];
  }
  bool hasV6MOps() const {
    return getSTI().getFeatureBits()[ARM::HasV6MOps];
  }
  bool hasV7Ops() const {
    return getSTI().getFeatureBits()[ARM::HasV7Ops];
  }
  bool hasV8Ops() const {
    return getSTI().getFeatureBits()[ARM::HasV8Ops];
  }
  bool hasV8MBaseline() const {
    return getSTI().getFeatureBits()[ARM::HasV8MBaselineOps];
  }
  bool hasV8MMainline() const {
    return getSTI().getFeatureBits()[ARM::HasV8MMainlineOps];
  }
  bool has8MSecExt() const {
    return getSTI().getFeatureBits()[ARM::Feature8MSecExt];
  }
  bool hasARM() const {
    return !getSTI().getFeatureBits()[ARM::FeatureNoARM];
  }
  bool hasDSP() const {
    return getSTI().getFeatureBits()[ARM::FeatureDSP];
  }
  bool hasD16() const {
    return getSTI().getFeatureBits()[ARM::FeatureD16];
  }
  bool hasV8_1aOps() const {
    return getSTI().getFeatureBits()[ARM::HasV8_1aOps];
  }
  bool hasRAS() const {
    return getSTI().getFeatureBits()[ARM::FeatureRAS];
  }

  void SwitchMode() {
    MCSubtargetInfo &STI = copySTI();
    uint64_t FB = ComputeAvailableFeatures(STI.ToggleFeature(ARM::ModeThumb));
    setAvailableFeatures(FB);
  }
  void FixModeAfterArchChange(bool WasThumb, SMLoc Loc);
  bool isMClass() const {
    return getSTI().getFeatureBits()[ARM::FeatureMClass];
  }

  /// @name Auto-generated Match Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "ARMGenAsmMatcher.inc"

  /// }

  OperandMatchResultTy parseITCondCode(OperandVector &);
  OperandMatchResultTy parseCoprocNumOperand(OperandVector &);
  OperandMatchResultTy parseCoprocRegOperand(OperandVector &);
  OperandMatchResultTy parseCoprocOptionOperand(OperandVector &);
  OperandMatchResultTy parseMemBarrierOptOperand(OperandVector &);
  OperandMatchResultTy parseInstSyncBarrierOptOperand(OperandVector &);
  OperandMatchResultTy parseProcIFlagsOperand(OperandVector &);
  OperandMatchResultTy parseMSRMaskOperand(OperandVector &);
  OperandMatchResultTy parseBankedRegOperand(OperandVector &);
  OperandMatchResultTy parsePKHImm(OperandVector &O, StringRef Op, int Low,
                                   int High);
  OperandMatchResultTy parsePKHLSLImm(OperandVector &O) {
    return parsePKHImm(O, "lsl", 0, 31);
  }
  OperandMatchResultTy parsePKHASRImm(OperandVector &O) {
    return parsePKHImm(O, "asr", 1, 32);
  }
  OperandMatchResultTy parseSetEndImm(OperandVector &);
  OperandMatchResultTy parseShifterImm(OperandVector &);
  OperandMatchResultTy parseRotImm(OperandVector &);
  OperandMatchResultTy parseModImm(OperandVector &);
  OperandMatchResultTy parseBitfield(OperandVector &);
  OperandMatchResultTy parsePostIdxReg(OperandVector &);
  OperandMatchResultTy parseAM3Offset(OperandVector &);
  OperandMatchResultTy parseFPImm(OperandVector &);
  OperandMatchResultTy parseVectorList(OperandVector &);
  OperandMatchResultTy parseVectorLane(VectorLaneTy &LaneKind, unsigned &Index,
                                       SMLoc &EndLoc);

  // Asm Match Converter Methods
  void cvtThumbMultiply(MCInst &Inst, const OperandVector &);
  void cvtThumbBranches(MCInst &Inst, const OperandVector &);

  bool validateInstruction(MCInst &Inst, const OperandVector &Ops);
  bool processInstruction(MCInst &Inst, const OperandVector &Ops, MCStreamer &Out);
  bool shouldOmitCCOutOperand(StringRef Mnemonic, OperandVector &Operands);
  bool shouldOmitPredicateOperand(StringRef Mnemonic, OperandVector &Operands);
  bool isITBlockTerminator(MCInst &Inst) const;

public:
  enum ARMMatchResultTy {
    Match_RequiresITBlock = FIRST_TARGET_MATCH_RESULT_TY,
    Match_RequiresNotITBlock,
    Match_RequiresV6,
    Match_RequiresThumb2,
    Match_RequiresV8,
    Match_RequiresFlagSetting,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "ARMGenAsmMatcher.inc"

  };

  ARMAsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
               const MCInstrInfo &MII, const MCTargetOptions &Options)
    : MCTargetAsmParser(Options, STI), MII(MII), UC(Parser) {
    MCAsmParserExtension::Initialize(Parser);

    // Cache the MCRegisterInfo.
    MRI = getContext().getRegisterInfo();

    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));

    // Not in an ITBlock to start with.
    ITState.CurPosition = ~0U;

    NextSymbolIsThumb = false;
  }

  // Implementation of the MCTargetAsmParser interface:
  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;
  bool ParseDirective(AsmToken DirectiveID) override;

  unsigned validateTargetOperandClass(MCParsedAsmOperand &Op,
                                      unsigned Kind) override;
  unsigned checkTargetMatchPredicate(MCInst &Inst) override;

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;
  unsigned MatchInstruction(OperandVector &Operands, MCInst &Inst,
                            uint64_t &ErrorInfo, bool MatchingInlineAsm,
                            bool &EmitInITBlock, MCStreamer &Out);
  void onLabelParsed(MCSymbol *Symbol) override;
};
} // end anonymous namespace
