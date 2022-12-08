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
                if newArray[row, col] then 1
        ]
        |> List.length

module Part2 =

    let scenicScore row col =
        [
            [for i in 0..col-1 do
                if data[row, i] >= data[row, col] then ("W", 1, data[row, i]) else ("W", 0, data[row, i])] |> List.rev
            [for i in col+1..lenCol-1 do
                if data[row, i] >= data[row, col] then ("E", 1, data[row, i]) else ("E", 0, data[row, i])] 
            [for i in 0..row-1 do
                if data[i, col] >= data[row, col] then ("N", 1, data[i, col]) else ("N", 0, data[i, col])] |> List.rev 
            [for i in row+1..lenRow-1 do
                if data[i, col] >= data[row, col] then ("S", 1, data[i, col]) else ("S", 0, data[i, col])]
        ]
        |> List.concat
        |> List.groupBy (fun (d,_,_) -> d)
        |> List.map (fun (_,xs) -> 
            xs
            |> List.map (fun (_,score,_) -> score)
            |> List.fold (fun acc item ->
                let (count, go) = acc
                if go && item = 0 then (count+1, true)
                elif go && item = 1 then (count+1, false)
                else (count, false)
            ) (0, true)
            |> fun (c,_) -> c
        )
        |> List.reduce (*)

    let newArray = Array2D.init lenRow lenCol (fun row col -> scenicScore row col)

    let maxScenicScore = 
        [
            for row in 1..lenRow-2 do
            for col in 1..lenCol-2 do
                newArray[row, col]
        ]
        |> List.max

    let result = maxScenicScore