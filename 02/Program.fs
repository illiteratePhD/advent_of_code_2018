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

let distanceMetric (a, b) = 
    match a = b with
    | true -> 0
    | false -> 1

let distance a b = Seq.sumBy(fun a -> distanceMetric a) <| Seq.zip a b

let rec allToAllDistanceSearch = function
    | [] -> ("", "")
    | (x::xs) -> match xs |> List.tryFind(fun s' -> (distance s' x = 1)) with
                 | Some y -> (x, y)
                 | None -> allToAllDistanceSearch(xs)

let fst (f, _) = f

let zipped (a, b) = Seq.zip a b

let rec removeNonDuplicate (i:list<char * char>, o:string) = 
    match i with
    | [] -> o
    | (x::xs) -> match distanceMetric x = 0 with 
                 | true -> removeNonDuplicate(xs, o + string (fst x))
                 | false -> removeNonDuplicate(xs, o)

[<EntryPoint>]
let main argv = 
    let twosAndThrees = countTwosAndThrees("input.txt" |> readTags, 0, 0)
    printfn "Hash for list = %d" (getHash twosAndThrees)

    let closestPair = "input.txt" |> readTags |> allToAllDistanceSearch 
    printfn "Closest pair: %s %s" <|| closestPair

    let correctChars = removeNonDuplicate(zipped closestPair |> Seq.toList, "")
    printfn "Tag without bad char: %s" correctChars

    0 // return an integer exit code
