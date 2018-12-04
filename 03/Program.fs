module Challengen03

open System.IO
open System.Text.RegularExpressions

type Point (x:int, y:int) =
    member this.X = x
    member this.Y = y

type Rect (x:int, y:int, width:int, height:int) = 
    
    member this.X = x
    member this.Y = y
    member this.Width = width
    member this.Height = height  

    member this.RectangleRegEx = @"^#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$"

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
        let rects = (parseRect x).Value::rects
        parseRectsFromInputs xs rects

[<EntryPoint>]
let main argv = 
    let fabricWidth = 1000
    let fabricHeight = 1000

    let entries = "input.txt" |> readInput
    let rectangles = parseRectsFromInputs entries List<Rect>.Empty
    printfn "Rectangles: %d" rectangles.Length
    printfn "Rectangle 2 [x: %d,y: %d,w: %d,h: %d]" (rectangles.Item(1).X) (rectangles.Item(1).Y) (rectangles.Item(1).Width) (rectangles.Item(1).Height)

    0 // return an integer exit code
