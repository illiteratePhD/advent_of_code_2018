module Challengen03

open System.IO
open System.Text.RegularExpressions
open System.Collections.Specialized

type Point (x:int, y:int) =
    member this.X = x
    member this.Y = y

type Rect (x:int, y:int, width:int, height:int) = 
    
    member this.X = x
    member this.Y = y
    member this.Width = width
    member this.Height = height  

    member this.IsInside (x, y) = 
        if (x < this.X) then 
            false
        elif (y < this.Y) then 
            false
        elif (x > this.X + this.Width - 1) then
            false
        elif (y > this.Y + this.Height - 1) then
            false
        else 
            true

let (|CompiledMatch|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern, RegexOptions.Compiled)
        if m.Success then Some [for x in m.Groups -> x]
        else None    

let parseRect s = 
    match s with
    |   CompiledMatch @"^#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$" [_; x; y; w; h] ->
        Some(new Rect(x.Value |> int, y.Value |> int, w.Value |> int, h.Value |> int))
    | _ -> 
        None
        
let readInput filePath = File.ReadLines(filePath) |> Seq.toList

let rec parseRectsFromInputs entries rects = 
    match entries with
    | [] -> rects
    | (x::xs) -> 
        let rects = rects @ [(parseRect x).Value]
        parseRectsFromInputs xs rects

        
let isPatchOverlapped (p:int*int, r:list<Rect>) = 
    let r = List.toArray r
    let mutable i = 0
    let mutable overlapCount = 0
    while overlapCount < 2 && i < r.Length do
        if (r.[i].IsInside(fst(p), snd(p))) then
            overlapCount <- overlapCount + 1
        i <- (i + 1)
    overlapCount > 1

let countOverlappedPatches w h rects = 
    let results = seq { for row in 0 .. h - 1 do
                           for col in 0 .. w - 1 do
                              yield match isPatchOverlapped((row, col), rects) with
                                    | true -> 1
                                    | false -> 0 }
    Seq.sum results

[<EntryPoint>]
let main argv = 
    let fabricWidth = 1000
    let fabricHeight = 1000

    let entries = "input.txt" |> readInput
    let rectangles = parseRectsFromInputs entries List<Rect>.Empty
    printfn "Overlap counts = %A" (countOverlappedPatches fabricWidth fabricHeight rectangles)

    0 // return an integer exit code
