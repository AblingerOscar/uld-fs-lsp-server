namespace ULD.Fs.LSP.Parsing

open ULD.Fs.DTOs
open ULD.Fs.LSP.Utils

[<Struct>]
type MarkerLane =
  { Positions: TextPosition list }
  static member Default: MarkerLane =
    { Positions = List.empty }

[<Struct>]
type Completion =
  { Position: TextPosition
    Text: string }

[<Struct>]
type ParseUnfinished =
  { IdentifierLane: MarkerLane
    PossibleCompletions: Completion list }

[<Struct>]
type Continuation =
  { Text: string }

[<Struct>]
type ParseFinished =
  { IdentifierLane: MarkerLane
    PossibleContinuations: Continuation list }

[<Struct>]
type ExpectedText =
  { Text: string }

[<Struct>]
type ParseFailure =
  { FailedAt: TextPosition
    ExpectedAfterwardsOneOf: ExpectedText list }

[<Struct>]
type ParseResult =
  | Unfinished of unfinishedSuccess: ParseUnfinished
  | Finished of finishedSuccess: ParseFinished
  | Failure of failure: ParseFailure

type Parser<'State> = interface
    abstract member ParseStartUntil: TextPosition -> MultilineText -> 'State
    abstract member ContinueParsingUntil: TextPosition -> 'State -> MultilineText -> 'State
    abstract member Finish: 'State -> ParseResult
  end

type ParserProvider<'Parser, 'State when 'Parser :> Parser<'State>> = interface
    abstract member SupportsLanguageDefinition: LanguageDefinition -> bool
  end
