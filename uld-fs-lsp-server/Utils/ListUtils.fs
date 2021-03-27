namespace ULD.Fs.LSP.Utils

module List =
  let public mapNth n map = List.mapi (fun idx value -> if idx <> n then value else map value)

  let public mapFirst map = mapNth 0 map
  let public mapLast map (list: 'a list) = mapNth list.Length map list
