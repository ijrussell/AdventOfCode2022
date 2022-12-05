open System.IO
open System.Collections.Generic

type StackArray(size:int) =
    let data = Array.init size (fun _ -> Stack<char>())

    let push destination value = data[destination-1].Push(value)
    
    let move count source destination =
        for _ in 1..count do
            let item = data[source-1].Pop()
            data[destination-1].Push(item)
    
    let peek () = 
        data
        |> Array.map (fun s -> s.Peek())
        //|> String

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

let decodeMoveToRow (input:string) =
    input.Split(' ')
    |> fun ary -> int ary[1], int ary[3], int ary[5]

let calculate (lines:string list) =
    let (initialState, moves) =
        let index =
            data
            |> List.findIndex (fun x -> x = "")
        let (state, move) = data |> List.splitAt index
        state |> List.rev, move |> List.tail
    let count = 
        initialState
        |> List.head
        |> getNumberOfStacks 
    let stackArray = StackArray(count)
    initialState
    |> List.tail
    |> List.map processStateRow
    |> List.iter (fun c -> c |> List.iter (fun (i,v) -> stackArray.Push(i, v)))
    moves
    |> List.map decodeMoveToRow
    |> List.iter (fun (c, s, d) -> stackArray.Move(c, s, d))
    stackArray.Peek()

let result = data |> calculate |> System.String


