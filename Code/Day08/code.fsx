open System.IO

let sourceData = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int 0 - 48))
    |> array2D

let lenRow = sourceData |> Array2D.length2 
let lenCol = sourceData |> Array2D.length1 

module Part1 =

    let isVisible row col =
        [
            let cellValue = sourceData[row, col]
            for i in 0..col-1 do
                if sourceData[row, i] >= cellValue then ("E", 1) 
            for i in col+1..lenCol-1 do
                if sourceData[row, i] >= cellValue then ("W", 1) 
            for i in 0..row-1 do
                if sourceData[i, col] >= cellValue then ("S", 1) 
            for i in row+1..lenRow-1 do
                if sourceData[i, col] >= cellValue then ("N", 1) 
        ]
        |> List.groupBy (fun (d,_) -> d)
        |> List.length < 4

    let visibleTrees = 
        let calculatedData = Array2D.init lenRow lenCol (fun row col -> isVisible row col)
        [
            for row in 0..lenRow-1 do
            for col in 0..lenCol-1 do
                if calculatedData[row, col] then 1
        ]
        |> List.length

module Part2 =

    let calculateScenicScore row col =
        [
            let cellValue = sourceData[row, col]
            [for i in 0..col-1 do
                if sourceData[row, i] >= cellValue then ("W", 1, sourceData[row, i]) else ("W", 0, sourceData[row, i])] |> List.rev
            [for i in col+1..lenCol-1 do
                if sourceData[row, i] >= cellValue then ("E", 1, sourceData[row, i]) else ("E", 0, sourceData[row, i])] 
            [for i in 0..row-1 do
                if sourceData[i, col] >= cellValue then ("N", 1, sourceData[i, col]) else ("N", 0, sourceData[i, col])] |> List.rev 
            [for i in row+1..lenRow-1 do
                if sourceData[i, col] >= cellValue then ("S", 1, sourceData[i, col]) else ("S", 0, sourceData[i, col])]
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

    let maxScenicScore = 
        let calculatedData = Array2D.init lenRow lenCol (fun row col -> calculateScenicScore row col)
        [
            for row in 1..lenRow-2 do
            for col in 1..lenCol-2 do
                calculatedData[row, col]
        ]
        |> List.max
