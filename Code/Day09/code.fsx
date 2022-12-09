open System.IO

let readLine (input:string) =
    let line = input.Split(' ')
    (line[0], int line[1])

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "sample-data.txt")
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
    let (hRow, hCol) = inputH
    let (tRow, tCol) = inputT
    let diffX, diffY = abs (hRow - tRow), abs (hCol - tCol) 
    // covered
    if inputH = inputT then inputT
    // next to
    elif (diffX = 0 && diffY = 1) || (diffX = 1 && diffY = 0) then inputT
    // diagonal to
    elif (diffX = 1 && diffY = 1) then inputT
    // away in lane of travel
    elif diffX = 0 && diffY = 2 && hCol > tCol then (tRow, tCol+1)
    elif diffX = 0 && diffY = 2 && hCol < tCol then (tRow, tCol-1)
    elif diffX = 2 && diffY = 0 && hRow > tRow then (tRow+1, tCol)
    elif diffX = 2 && diffY = 0 && hRow < tRow then (tRow-1, tCol)
    // away not in lane of travel
    elif diffX = 1 && diffY = 2 && hCol > tCol && hRow > tRow then (tRow+1, tCol+1)
    elif diffX = 1 && diffY = 2 && hCol > tCol && hRow < tRow then (tRow-1, tCol+1)
    elif diffX = 1 && diffY = 2 && hCol < tCol && hRow > tRow then (tRow+1, tCol-1)
    elif diffX = 1 && diffY = 2 && hCol < tCol && hRow < tRow then (tRow-1, tCol-1)
   
    elif diffX = 2 && diffY = 1 && hRow > tRow && hCol > tCol then (tRow+1, tCol+1)
    elif diffX = 2 && diffY = 1 && hRow > tRow && hCol < tCol then (tRow+1, tCol-1)
    
    elif diffX = 2 && diffY = 1 && hRow < tRow && hCol > tCol then (tRow-1, tCol+1)
    elif diffX = 2 && diffY = 1 && hRow < tRow && hCol < tCol then (tRow-1, tCol-1)
    // else
    else inputT

let assertTail = makeMoveTail (1,0) (0,0) = (0,0)

let processInstruction (current:int * int) (move:string * int) =
    let direction, distance = move
    [for _ in 1..distance do direction]
    |> List.fold (fun acc item ->
        let current, history = acc
        let latest = makeMove current item
        latest, latest::history) (current, [])
    
module Part2 =

    let calculateHead (directions:(string * int) list) =
        directions
        |> List.fold (fun acc item ->
            let current, history = acc
            let latest, latestMoves = processInstruction current item
            latest, latestMoves@history
        ) ((0, 0), [])

    let calculateTail (positions:(int * int) list) =
        positions
        |> List.fold (fun acc item ->
            let current, history = acc
            let latest = makeMoveTail item current 
            latest, latest::history
        ) ((0,0), [])
    
    let result = 
        data 
        |> calculateHead
        |> fun (_,positions) -> positions
        |> List.rev
        |> calculateTail
        |> fun (_,x) -> x
        |> List.rev
        |> List.distinct
        |> List.length

