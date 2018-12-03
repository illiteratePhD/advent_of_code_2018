module Challenge02

open System.IO

// Read data

let readTags filePath = File.ReadLines(filePath)

[<EntryPoint>]
let main argv = 
    "input.txt" |> readTags |> Seq.toList |> List.iter (printfn "%A ")
    0 // return an integer exit code
