open System.IO
open System

let getInput path = File.ReadAllText(path)

let utfToByteArr (s:string) = Text.Encoding.ASCII.GetBytes s
let byteArrToUtf (b:array<byte>) = Text.Encoding.UTF8.GetString b

// Diff between small and caps in ASCII is 32. Use that to check reactants
let areReactants (a:byte, b:byte) = Math.Abs((int a) - (int b)) = 32

let removeReactants (arr:array<byte>) = 
    let result : byte array = Array.zeroCreate arr.Length
    result.[0] <- arr.[0]
    let mutable resInd = 0
    let mutable highInd = 1

    while highInd < arr.Length do
        while highInd < arr.Length && areReactants(result.[resInd], arr.[highInd]) do
            if resInd - 1 < 0 then
                resInd <- 0
                highInd <- highInd + 2
                result.[0] <- arr.[highInd - 1]
            else
                Array.Clear(result, resInd, 1)
                resInd <- resInd - 1
                highInd <- highInd + 1
        if highInd < arr.Length then
            resInd <- resInd + 1
            result.[resInd] <- arr.[highInd]
            highInd <- highInd + 1
    result

let ignore (a:byte, b:byte) = 
    let diff = Math.Abs((int a) - (int b))
    (diff = 0 || diff = 32)

let removeIgnored (arr:array<byte>, ignoreChar:byte) = 
    arr |> Array.where(fun e -> ignore(e, ignoreChar) |> not)

let pruneAndConvert (arr:array<byte>) =
    let index = arr |> Array.findIndex(fun e -> e = 0uy)
    byteArrToUtf arr.[0..index - 1]

[<EntryPoint>]
let main argv = 

    // Testing the algorithm
    let testString = "dabAcCaCBAcCcaDA"B
    let testResult = removeReactants testString
    
    printfn "Result for demo data: %A" (pruneAndConvert testResult)

    // Running real data part 1
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let polyString = getInput "input.txt"
    let polyAscii =  utfToByteArr polyString
    let realResult = removeReactants polyAscii
    stopWatch.Stop()
    printfn "Length of result for real data: %A. Time of execution is: %f" (pruneAndConvert realResult).Length stopWatch.Elapsed.TotalMilliseconds

    // Evaluating best char to drop for part 2
    let mutable minLength = Int32.MaxValue
    for i in 65uy..90uy do
        printf "Testing character %A / %A.\n" (byteArrToUtf([|i|])) (byteArrToUtf([|i + 32uy|]))
        let unitRemoved = removeIgnored(polyAscii, i)
        let evalResult = removeReactants(unitRemoved)
        let length = (pruneAndConvert evalResult).Length
        if length < minLength then minLength <- length
        printf "Resulting length: %A\n" length
    
    printfn "Minimum length of evaluation: %A." minLength

    0 // return an integer exit code
