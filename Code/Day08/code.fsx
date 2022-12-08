open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int 0 - 48))
    |> array2D

let lenRow = data |> Array2D.length2 
let lenCol = data |> Array2D.length1 

let isVisible row col =
    [
        for i in 0..col-1 do
            if data[row, i] >= data[row, col] then ("E", 1) 
        for i in col+1..lenCol-1 do
            if data[row, i] >= data[row, col] then ("W", 1) 
        for i in 0..row-1 do
            if data[i, col] >= data[row, col] then ("S", 1) 
        for i in row+1..lenRow-1 do
            if data[i, col] >= data[row, col] then ("N", 1) 
    ]
    |> List.groupBy (fun (d,_) -> d)
    |> List.length < 4

let outerTotal = 
    ((lenCol - 1) + (lenRow - 1)) * 2

let total = 
    [
        for row in 1..lenRow-2 do
        for col in 1..lenCol-2 do
            if isVisible row col then Some (row, col) else None
    ]
    |> List.choose id
    |> List.length
    |> fun t -> t + outerTotal

