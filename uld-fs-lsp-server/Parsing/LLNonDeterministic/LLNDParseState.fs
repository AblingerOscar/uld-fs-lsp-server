namespace ULD.Fs.LSP.Parsing.LLNonDeterministic

open ULD.Fs.DTOs
open ULD.Fs.LSP.Parsing
open ULD.Fs.LSP.Utils
open ULD.Fs.LSP.Utils.ActivePatternUtils

type BuiltInAction =
  | Identifier
  | Declaration
  | Definition
  | Implementation
  | IdentifierTypeInner
  | IdentifierTypeSet of string
  | IdentifierKindSet of string
  | FoldingStart
  | FoldingEnd
  | EnvironmentPop
  | EnvironmentPushFixed of string option
  | EnvironmentPushInner of string
  | EnvironmentImportFixed of string option
  | EnvironmentImportInner of string
  static member tryParseActionString cmd =
    match cmd with
    | "identifier" -> Some Identifier
    | "declaration" -> Some Declaration
    | "definition" -> Some Definition
    | "implementation" -> Some Implementation
    | "identifierType inner" -> Some IdentifierTypeInner
    | StringStartsWith "identifierType set " tail -> Some (IdentifierTypeSet tail)
    | StringStartsWith "identifierKind set " tail -> Some (IdentifierKindSet tail)
    | "folding start" -> Some FoldingStart
    | "folding end" -> Some FoldingEnd
    | "environment pop" -> Some EnvironmentPop
    | StringStartsWith "environment push fixed" tail ->
      Some (EnvironmentPushFixed (if tail.Length <> 0 then Some (tail.Trim()) else None))
    | StringStartsWith "environment push inner" tail ->
      Some (EnvironmentPushInner (if tail.Length <> 0 then (tail.Trim()) else "{inner}"))
    | StringStartsWith "environment import fixed" tail ->
      Some (EnvironmentImportFixed (if tail.Length <> 0 then Some (tail.Trim()) else None))
    | StringStartsWith "environment import inner" tail ->
      Some (EnvironmentImportInner (if tail.Length <> 0 then (tail.Trim()) else "{inner}"))
    | _ -> None

type ProductionSymbol =
  | Reference of string option list
  | ParseChar of (char -> bool)
  | StandardAction of BuiltInAction
  | UserAction of string
  static member ofSymbol (symbol: SymbolDefinition) = [
      // todo: cache parsers (especially common characters)
      match symbol with
      | Action cmd ->
        yield
          BuiltInAction.tryParseActionString cmd
          |> Option.map StandardAction
          |> Option.defaultValue<ProductionSymbol> (UserAction cmd)
      | NonTerminal referencedRule -> yield Reference [ Some referencedRule ]
      | OneOf (allowNone, options) ->
        if allowNone then
          yield Reference (None::(options |> List.map Some))
        else
          yield Reference (options |> List.map Some)
      | Whitespace -> yield ParseChar System.Char.IsWhiteSpace
      | LineEnd -> yield ParseChar ((=) '\n') // new lines are unified by the MultilineText type
      | String str ->
        yield!
          str
          |> Seq.map (fun ch -> ParseChar ((=) ch))
      | Digit -> yield ParseChar System.Char.IsDigit
      | Letter -> yield ParseChar System.Char.IsLetter
      | LetterOrDigit -> yield ParseChar System.Char.IsLetterOrDigit
      | LowercaseLetter -> yield ParseChar System.Char.IsLower
      | UppercaseLetter -> yield ParseChar System.Char.IsUpper
      | Character -> yield ParseChar (fun _ -> true)
      | CharacterOf chars ->
        let charSet = chars |> Set.ofList
        yield ParseChar charSet.Contains
      | CharacterExcept chars ->
        let charSet = chars |> Set.ofList
        yield ParseChar (charSet.Contains >> not)
    ]

type Lanes =
  { IdentifierLane: MarkerLane }
  static member Default: Lanes =
    { IdentifierLane = MarkerLane.Default }

type Production =
  { Symbols: ProductionSymbol list
    Lanes: Lanes }
  static member ofSymbols (symbols: SymbolDefinition list) =
    { Symbols = List.collect ProductionSymbol.ofSymbol symbols
      Lanes = Lanes.Default }

type LLNDParseState =
  { TextPosition: TextPosition
    CurrentProductions: Production list }
  static member Default: LLNDParseState =
    { TextPosition = TextPosition (0, 0)
      CurrentProductions = List.empty }
