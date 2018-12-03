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

let rec firstDuplicateFrequency (currentIndex:int, deltas:List<int>, visitedFrequencies:Set<int>, currentFrequency: int) = 
    match visitedFrequencies.Contains currentFrequency with
    | true -> currentFrequency
    | false -> 
        let nextIndex = match deltas.Length = (currentIndex + 1) with
                        | true -> 0
                        | false -> (currentIndex + 1)
        firstDuplicateFrequency (nextIndex, deltas, (visitedFrequencies.Add currentFrequency), (currentFrequency + deltas.[currentIndex]))

let rec firstDuplicateFrequency' (deltas:List<int>, visitedFrequencies:Set<int>, currentFrequency: int) = 
    match visitedFrequencies.Contains currentFrequency with
    | true -> currentFrequency
    | false -> match deltas with
               | [] -> 0
               | x::xs -> firstDuplicateFrequency' (xs @ [x], (visitedFrequencies.Add currentFrequency), (currentFrequency + x))

[<EntryPoint>]
let main argv = 
    let deltas = getListOfDeltas "input.txt"
    printfn "Sum of deltas is: %d" (sumOfDeltas deltas)
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    printfn "First duplicate frequency (method 1) is: %d" (firstDuplicateFrequency' (deltas, Set.empty, 0))
    stopWatch.Stop()
    printfn "Method 1 time: %f" stopWatch.Elapsed.TotalMilliseconds
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    printfn "First duplicate frequency (method 2) is: %d" (firstDuplicateFrequency (0, deltas, Set.empty, 0))
    stopWatch.Stop()
    printfn "Method 2 time: %f" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
