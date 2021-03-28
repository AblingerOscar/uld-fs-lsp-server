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
    let transformProductionInfo (production: Production) =
      if List.isEmpty production.Symbols then
        Finished { IdentifierLane = production.Lanes.IdentifierLane
                   PossibleContinuations = List.empty }
      else
        // Unexpected EOI
        // (more input could have the parsing finish successfully)
        Unfinished { IdentifierLane = production.Lanes.IdentifierLane
                     PossibleCompletions = List.empty }

    if List.isEmpty state.CurrentProductions then
      // failed at some point
      Failure { FailedAt = state.TextPosition
                ExpectedAfterwardsOneOf = List.empty }
    else
      state.CurrentProductions
      |> List.map transformProductionInfo
      |> parserConfig.ChooseFavouredParseResult

  member this.ParseStateUntil endPos mText =
    this.ContinueParsingUntil endPos { LLNDParseState.Default with CurrentProductions = startProductions } mText


  interface Parser<LLNDParseState> with
    member this.ContinueParsingUntil endPos state mText = this.ContinueParsingUntil endPos state mText
    member this.Finish state = this.Finish state
    member this.ParseStartUntil endPos mText = this.ParseStateUntil endPos mText
