open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type MoveModel = CrateMover9000 | CrateMover9001

type StackArray(size:int, model:MoveModel) =
    let data = Array.init size (fun _ -> Stack<char>())

    let push destination value = data[destination-1].Push(value)
    
    let move count source destination =
        match model with
        | CrateMover9000 ->
            for _ in 1..count do
                let item = data[source-1].Pop()
                data[destination-1].Push(item)
        | CrateMover9001 ->
            [ for _ in 1..count do data[source-1].Pop() ]    
            |> List.rev
            |> List.iter (fun item ->
                data[destination-1].Push(item))
    
    let peek () = 
        data
        |> Array.map (fun s -> s.Peek())
        |> String

    member _.Push(destination:int, value:char) = push destination value
    member _.Move(count:int, source:int, destination:int) = move count source destination
    member _.Peek() = peek ()

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList

let processStateRow (line:string) = 
    line
    |> Seq.chunkBySize 4
    |> Seq.mapi (fun i c -> i+1, c[1])
    |> Seq.filter (fun (_,c) -> c <> ' ')
    |> Seq.toList

let getNumberOfStacks (line:string) = 
    line
    |> Seq.filter (fun c -> c <> ' ')
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.max

let decodeMoveToRow (move:string) =
    let pattern = "move (?<count>\d+) from (?<source>\d+) to (?<destination>\d+)"
    let matched = Regex.Match(move, pattern)
    let getValue (group:string) = int matched.Groups.[group].Value
    getValue "count", getValue "source", getValue "destination"

let splitFile (lines:string list) =
    let index =
        lines
        |> List.findIndex (fun x -> x = "")
    let (state, move) = data |> List.splitAt index
    state |> List.rev, move |> List.tail

let calculate (model:MoveModel) (lines:string list) =
    let (initialState, moves) = lines |> splitFile
    let stackArray = 
        initialState
        |> List.head
        |> getNumberOfStacks 
        |> fun count -> StackArray(count, model)
    initialState
    |> List.tail
    |> List.map processStateRow
    |> List.iter (fun c -> c |> List.iter (fun (i,v) -> stackArray.Push(i, v)))
    moves
    |> List.map decodeMoveToRow
    |> List.iter (fun (c, s, d) -> stackArray.Move(c, s, d))
    stackArray.Peek()

module Part1 =

    let result = data |> calculate CrateMover9000

module Part2 =

    let result = data |> calculate CrateMover9001
