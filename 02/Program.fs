module Challenge02

open System.IO
open System.Collections.Generic

// Read data

let readTags filePath = File.ReadLines(filePath)

let getCharacterCounts (s:string, m:Dictionary<char, int>) = 
    s |> Seq.iter (fun c -> match m.ContainsKey c with
                            | true -> m.[c] <- (m.[c] + 1)
                            | false -> m.Add(c, 1))

[<EntryPoint>]
let main argv = 
    //"input.txt" |> readTags |> Seq.toList |> List.iter (printfn "%A ")
    0 // return an integer exit code
