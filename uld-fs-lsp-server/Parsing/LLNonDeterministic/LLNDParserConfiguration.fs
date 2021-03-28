namespace ULD.Fs.LSP.Parsing.LLNonDeterministic

  open System
  open ULD.Fs.LSP.Parsing

  type LLNDParserConfiguration =
    { ChooseFavouredParseResult: ParseResult list -> ParseResult }
    static member Default =
      let chooseFavouredParseResult parseResults: ParseResult =
        parseResults// prioritize res1: -1
        |> List.sortWith (fun res1 res2 ->
          match res1, res2 with
          | Failure f1, Failure f2 ->
            if f1 = f2 then 0
            elif f1.FailedAt.IsBefore f2.FailedAt then 1
            else -1
          | Failure _, Unfinished _ -> 1
          | Unfinished _, Failure _ -> -1
          | Unfinished u1, Unfinished u2 ->
            let cntS1 = u1.IdentifierLane.Positions |> List.length
            let cntS2 = u2.IdentifierLane.Positions |> List.length
            if cntS1 = cntS2 then 0
            elif cntS1 < cntS2 then 1
            else -1
          | Failure _, Finished _ -> 1
          | Finished _, Failure _ -> -1
          | Unfinished _, Finished _ -> 1
          | Finished _, Unfinished _ -> -1
          | Finished f1, Finished f2 ->
            let cntS1 = f1.IdentifierLane.Positions |> List.length
            let cntS2 = f2.IdentifierLane.Positions |> List.length
            if cntS1 = cntS2 then 0
            elif cntS1 < cntS2 then 1
            else -1
          )
        |> List.head

      {
        /// Choose the favoured parse result from a list of
        /// possible parse results.
        /// This list will never be empty – it might, however
        /// contain only one element.
        ChooseFavouredParseResult = chooseFavouredParseResult
      }
