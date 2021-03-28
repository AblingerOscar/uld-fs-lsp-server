namespace ULD.Fs.LSP.Parsing.LLNonDeterministic

open ULD.Fs.DTOs
open ULD.Fs.LSP.Parsing
open ULD.Fs.LSP.Utils

type LLNDParser(langDef: LanguageDefinition, parserConfig: LLNDParserConfiguration) =
  let productionMap =
    langDef.rules
    |> Map.map (fun _ -> Production.ofSymbols)
  let startProductions =
    langDef.startRules
    |> List.map (fun rule -> productionMap.[rule])

  member this.ContinueParsingUntil (endPos: TextPosition) (state: LLNDParseState) (mText: MultilineText) =
    failwith ""

  member this.Finish (state: LLNDParseState) =
    let anyProductionIsFinished =
      state.CurrentProductions
      |> List.exists (fun prod -> prod.Symbols.Length = 0)

    if List.isEmpty state.CurrentProductions then
      // failed at some point
      Failure { FailedAt = state.TextPosition
                ExpectedAfterwardsOneOf = List.empty }
    elif not anyProductionIsFinished then
      // Unexpected EOI
      // (more input could have the parsing finish successfully)
      Unfinished { IdentifierLane = state.Lanes.IdentifierLane
                   PossibleCompletions = List.empty }
    else
      Finished { IdentifierLane = state.Lanes.IdentifierLane
                 PossibleContinuations = List.empty }

  member this.ParseStateUntil endPos mText =
    this.ContinueParsingUntil endPos { LLNDParseState.Default with CurrentProductions = startProductions } mText


  interface Parser<LLNDParseState> with
    member this.ContinueParsingUntil endPos state mText = this.ContinueParsingUntil endPos state mText
    member this.Finish state = this.Finish state
    member this.ParseStartUntil endPos mText = this.ParseStateUntil endPos mText
