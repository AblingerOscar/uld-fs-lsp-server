namespace ULD.Fs.LSP.Parsing

open ULD.Fs.LSP.Utils

[<Struct>]
type MarkerLane =
  { Positions: TextPosition list }

[<Struct>]
type Completion =
  { Position: TextPosition
    Text: string }

[<Struct>]
type ParseSuccess =
  { IdentifierLane: MarkerLane
    PossibleCompletions: Completion }

[<Struct>]
type ExpectedText =
  { Text: string }

[<Struct>]
type ParseFailure =
  { FailedAt: TextPosition
    ExpectedAfterwards: ExpectedText }

[<Struct>]
type ParseResult =
  | Success of success: ParseSuccess
  | Failure of failure: ParseFailure

type Parser<'State> = interface
    abstract member parseRange: TextRange -> MultilineText -> 'State
    abstract member continueParsingFrom: 'State -> MultilineText -> 'State

    abstract member finish: 'State -> ParseResult
  end

