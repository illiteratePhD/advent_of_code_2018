open System.IO
open System

let getInput path = File.ReadAllText(path)

let utfToByteArr (s:string) = Text.Encoding.ASCII.GetBytes s
let byteArrToUtf (b:array<byte>) = Text.Encoding.UTF8.GetString b

// Diff between small and caps in ASCII is 32. Use that to check reactants
let areReactants (a:byte, b:byte) = Math.Abs((int a) - (int b)) = 32

let removeReactants (arr:array<byte>) = 
    let result : byte array = Array.zeroCreate arr.Length
    let mutable resInd = 0
    let mutable lowInd = 0
    let mutable highInd = 1

    while highInd < arr.Length do
        printf "Checking %A and %A\n" (byteArrToUtf [|arr.[lowInd]|]) (byteArrToUtf [|arr.[highInd]|])
        while highInd < arr.Length && areReactants(arr.[lowInd], arr.[highInd]) do
            printf "Removing %A and %A\n" (byteArrToUtf [|arr.[lowInd]|]) (byteArrToUtf [|arr.[highInd]|])
            resInd <- if resInd - 1 < 0 then 0 else resInd - 1
            highInd <- if lowInd - 1 < 0 then highInd + 2 else highInd + 1
            lowInd <- if lowInd - 1 < 0 then highInd - 1 else lowInd - 1
            printf "Checking %A and %A\n" (byteArrToUtf [|arr.[lowInd]|]) (byteArrToUtf [|arr.[highInd]|])
        result.[resInd] <- arr.[lowInd]
        printf "Result: %A \n" (byteArrToUtf result.[0..10])
        resInd <- resInd + 1
        lowInd <- highInd
        highInd <- highInd + 1
    
    result.[resInd] <- arr.[lowInd]
    result

let pruneAndConvert (arr:array<byte>) =
    let index = arr |> Array.findIndex(fun e -> e = 0uy)
    byteArrToUtf arr.[0..index - 1]

[<EntryPoint>]
let main argv = 

    // Testing the algorithm
    //let testString = "dabAcCaCBAcCcaDA"B
    //let testResult = removeReactants testString
    
    //printfn "Result for demo data: %A" (pruneAndConvert testResult)

    // Running real data part 1
    let polyString = getInput "input.txt"
    let polyAscii =  utfToByteArr polyString
    let realResult = removeReactants polyAscii
    
    printfn "Length of result for real data: %A" (pruneAndConvert realResult).Length

    0 // return an integer exit code
