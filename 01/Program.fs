// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



module Challenge01

open System.IO

// Parse data

let readDeltas filePath = File.ReadLines(filePath)

let tryParseInt s =
    match System.Int32.TryParse s with
    | true, n -> Some n
    | false, _ -> None

let linesToList readValues = 
    Seq.choose tryParseInt readValues |> Seq.toList

let getListOfDeltas s = 
    readDeltas s |> linesToList

// First assignment, sum of deltas

let rec sumOfDeltas = function
    | [] -> 0
    | x::xs -> x + sumOfDeltas xs

// Second assignment, first recurring frequency

let rec firstDuplicateFrequency (deltas:List<int>, visitedFrequencies:Set<int>, currentFrequency: int) = 
    match visitedFrequencies.Contains currentFrequency with
    | true -> currentFrequency
    | false -> match deltas with
               | [] -> 0
               | x::xs -> firstDuplicateFrequency (xs @ [x], (visitedFrequencies.Add currentFrequency), (currentFrequency + x))

[<EntryPoint>]
let main argv = 
    let deltas = getListOfDeltas "input.txt"
    printfn "Sum of deltas is: %d" (sumOfDeltas deltas)
    printfn "First duplicate frequency is: %d" (firstDuplicateFrequency (deltas, Set.empty, 0))
    0 // return an integer exit code
