// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



module SumOfDeltas

open System.IO

let readDeltas filePath = File.ReadLines(filePath)

let tryParseInt s =
    match System.Int32.TryParse s with
    | true, n -> Some n
    | false, _ -> None

let linesToList readValues = 
    Seq.choose tryParseInt readValues |> Seq.toList

let rec addDeltas = function
    | [] -> 0
    | x::xs -> x + addDeltas xs

let deltas = readDeltas("input.txt");

[<EntryPoint>]
let main argv = 
    printfn "Sum of deltas is: %d" (readDeltas "input.txt" |> linesToList |> addDeltas)
    0 // return an integer exit code
