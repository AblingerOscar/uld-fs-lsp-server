module ULD.Fs.LSP.Utils.ActivePatternUtils

open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
  let m = Regex(regex).Match str
  if m.Success then
    Some (List.tail [ for x in m.Groups -> x.Value ])
  else
    None

let (|StringStartsWith|_|) (prefix: string) (str: string) =
  if str.StartsWith prefix then
    Some (str.Substring(prefix.Length))
  else
    None
