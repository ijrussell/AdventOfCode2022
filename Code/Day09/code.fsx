open System.IO

let readLine (input:string) =
    let line = input.Split(' ')
    (line[0], int line[1])

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList
    |> List.map readLine

let makeMove (input:int * int) (direction:string) =
    let (row, col) = input
    match direction with
    | "R" -> row+1, col 
    | "L" -> row-1, col 
    | "U" -> row, col+1 
    | "D" -> row, col-1 
    | dir -> failwith $"Unknown direction: {dir}"
    
let makeMoveTail (inputH:int * int) (inputT:int * int) =
    let (hRow, hCol), (tRow, tCol) = inputH, inputT
    let diffX, diffY =
        match hRow-tRow, hCol-tCol with
        | 2, 0 -> 1, 0
        | -2, 0 -> -1, 0
        | 0, 2 -> 0, 1
        | 0, -2 -> 0, -1
        | 1, 2 | 2, 1 | 2, 2 -> 1, 1
        | 2, -1 | 1, -2 | 2, -2 -> 1, -1
        | -1, -2 | -2, -1 | -2, -2 -> -1, -1
        | -2, 1 | -1, 2 | -2, 2 -> -1, 1
        | x, y when abs x < 2 && abs y < 2 -> 0, 0
        | ex -> failwith $"Unexpected values: {ex}"
    (tRow + diffX, tCol + diffY)

let processInstruction (current:int * int) (move:string * int) =
    let direction, distance = move
    [for _ in 1..distance do direction]
    |> List.fold (fun acc item ->
        let position, history = acc
        let latest = makeMove position item
        latest, latest::history) (current, [])
    
let calculateHead (directions:(string * int) list) =
    directions
    |> List.fold (fun acc item ->
        let current, history = acc
        let latest, latestMoves = processInstruction current item
        latest, latestMoves@history
    ) ((0, 0), [])
    |> fun (_, items) -> items |> List.rev

let calculateTails (tails:string list) (positions:(int * int) list) =
    tails
    |> List.fold (fun acc _ -> 
        acc
        |> List.fold (fun acc item ->
            let current, history = acc
            let latest = makeMoveTail item current 
            latest, latest::history
        ) ((0,0), [])
        |> fun (_, items) -> items |> List.rev
    ) positions

let result (tails:string list) = 
    data 
    |> calculateHead
    |> calculateTails tails
    |> List.distinct 
    |> List.length

let part1 = ["T"] |> result 
let part2 = [1..9] |> List.map string |> result 
