open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int 0 - 48))
    |> array2D

let lenRow = data |> Array2D.length2 
let lenCol = data |> Array2D.length1 

module Part1 =

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

    let newArray = Array2D.init lenRow lenCol (fun row col -> isVisible row col)

    let total = 
        [
            for row in 0..lenRow-1 do
            for col in 0..lenCol-1 do
                if newArray[row, col] then Some () else None
        ]
        |> List.choose id
        |> List.length

