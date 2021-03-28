namespace ULD.Fs.LSP.Utils

open System
open FSharpPlus

[<Struct>]
type TextPosition =
  val Line: int
  /// The column of the character after the cursor.
  /// Note that this means that the column can go from
  /// 0 up to (including) the length of the line (signifying
  /// the cursor being after the last character)
  val Column: int

  new(line, column) = { Line = line; Column = column }

  override this.ToString () = $"TextPosition({this.Line}, {this.Column})"
  member this.IsBefore (other: TextPosition) =
    match this.Line - other.Line, this.Column - other.Column with
    | i, _ when i < 0 -> true
    | i, _ when i > 0 -> false
    | 0, i when i < 0 -> true
    | 0, i when i > 0 -> false
    | _ -> false

/// <summary>
/// Due to the definition of
/// <see cref="ULD.Fs.LSP.Utils.TextPosition">TextPosition</see>
/// a text range from (0,1)->(0,3) will include
/// two characters (the second and third).
/// </summary>
[<Struct>]
type TextRange =
  val Start: TextPosition
  val End: TextPosition

  new(start, end') = { Start = start; End = end' }
  override this.ToString () = $"TextRange({this.Start}, {this.End})"

type MultilineText =
  struct
    val private rawText: string list
    new(text: string list) = { rawText = text }
    override this.ToString () = $"""MultilineText({this.AsText |> String.concat "\n"})"""

    static member Empty = MultilineText List.Empty

    member this.AsText = this.rawText

    member this.GetSlice (start, finish) =
      let (startLine, startCol) = defaultArg start (0, 0)
      let (finishLine, finishCol) = defaultArg finish (0, 0)

      if startLine > finishLine || (startLine = finishLine && startCol >= finishCol) then
        MultilineText.Empty
      else
        this.AsText
        |> List.skip (startLine - 1)
        |> List.take (finishLine - startLine + 1)
        |> List.mapFirst (fun str -> str.Substring startCol)
        |> List.mapLast (fun str -> str.Substring (0, finishCol))
        |> MultilineText

    static member hasPosition line column (mText: MultilineText) =
      line < mText.AsText.Length && column < mText.AsText.[line].Length

    static member item line column (mText: MultilineText) =
      mText.AsText
      |> List.item line
      |> String.item column

    static member tryItem line column (mText: MultilineText) =
      mText.AsText
      |> List.tryItem line
      |> Option.bind (String.tryItem column)

    static member numberOfLines (mText: MultilineText) = mText.AsText.Length
    static member lineLength line (mText: MultilineText) = mText.AsText.[line].Length
    static member tryLineLength line (mText: MultilineText) =
      mText.AsText
      |> List.tryItem line
      |> Option.map (String.length)

    static member getTextRange (range: TextRange) (mText: MultilineText) =
        let start = range.Start.Line, range.Start.Column
        let end' = range.End.Line, range.End.Column

        mText.[start..end']
  end
