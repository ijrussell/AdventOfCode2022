open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList

let readLine (input:string) =
    let line = input.Split(' ')
    (line[0], int line[1])

let makeMove (input:int * int) (direction:string) =
    let (row, col) = input
    match direction with
    | "R" -> row + 1, col 
    | "L" -> row - 1, col 
    | "U" -> row, col + 1 
    | "D" -> row, col - 1 
    | dir -> failwith $"Unknown direction: {dir}"
    
let makeTMove (inputH:int * int) (inputT:int * int) (direction:string) =
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
    else makeMove inputT direction

let makeHMove (input:int * int) (direction:string) =
    makeMove input direction
    
let processMove (hPos:int * int) (tPos:int * int) (move:string * int) =
    let direction, distance = move
    let steps = [for _ in 1..distance do direction]
    let rec loop hacc tacc acc rem =
        match rem with
        | [] -> hacc, tacc, acc
        | head::tail -> 
            let hMove = makeHMove hacc head
            let tMove = makeTMove hMove tacc head
            loop hMove tMove ((hMove,tMove)::acc) tail 
    loop hPos tPos [] steps
    
let calculate (input:string list) =
    input
    |> List.fold (fun acc item ->
        let (hrow, hcol), (trow, tcol), tMoves = acc
        let move = readLine item
        let (hPos, tPos, latestMoves) = processMove (hrow, hcol) (trow, tcol) move
        (hPos, tPos, latestMoves::tMoves)
    ) ((0, 0), (0, 0), [])

let result = 
    data 
    |> calculate
    |> fun (_, _, moves) -> 
        moves
        |> List.concat
        |> List.map (fun (_,t) -> t)
        |> List.distinct
        |> List.length

