module Challenge02

open System.IO
open System.Collections.Generic

// Read data

let readTags filePath = File.ReadLines(filePath) |> Seq.toList

let getCharacterCounts (s:string, m:Dictionary<char, int>) = 
    s |> Seq.iter (fun c -> match m.ContainsKey c with
                            | true -> m.[c] <- (m.[c] + 1)
                            | false -> m.Add(c, 1))

let plusOneIfExist (value, collection, i) =
    match collection |> Seq.contains(i) with
    | true -> value + 1
    | false -> value

let rec countTwosAndThrees (tags, twos, threes) =
    match tags with
    | [] -> (twos, threes)
    | (x::xs) -> 
        let dict = new Dictionary<char, int>()
        getCharacterCounts(x, dict)
        countTwosAndThrees(xs, plusOneIfExist(twos, dict.Values, 2), plusOneIfExist(threes, dict.Values, 3))
 
let getHash (a, b) = a * b 

[<EntryPoint>]
let main argv = 
    let twosAndThrees = countTwosAndThrees("input.txt" |> readTags, 0, 0)
    printfn "Hash for list = %d" (getHash twosAndThrees)
    0 // return an integer exit code
