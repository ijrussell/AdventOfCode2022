open System.IO
open System.Text.RegularExpressions

type FileSys =
    | Folder of Name:string list * Current:string list
    | File of Name:string list * Size:int
    | NotRequired

type Directory =
    | Root
    | Parent
    | Named of Name:string

type Action =
    | CreateFile of Name:string * Size:int
    | CreateFolder of Name:string
    | ChangeDirectory of Directory:Directory
    | ListContents

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

let (|IsFile|_|) input =
    match input with
    | ParseRegex "(?<size>\d+) (?<file>\D+)" [size;name] -> Some (size, name)
    | _ -> None

let (|IsFolder|_|) input =
    match input with
    | ParseRegex "dir (?<name>\D+)" [name] -> Some name
    | _ -> None

let (|IsChangeDirectory|_|) input =
    match input with
    | "$ cd /" -> Some Root
    | "$ cd .." -> Some Parent
    | ParseRegex "\$ cd (?<name>\D+)" [name] -> Some (Named name)
    | _ -> None

let (|IsListContents|_|) input =
    match input with
    | "$ ls" -> Some ()
    | _ -> None

let matchInput (input:string) =
    match input with 
    | IsFile (size, name) -> CreateFile (Name = name, Size = int size)
    | IsFolder name -> CreateFolder (Name = name)
    | IsChangeDirectory name -> ChangeDirectory (Directory = name)
    | IsListContents -> ListContents
    | _ -> failwith $"Unknown action: {input}"

let processInputFile (input:string[]) =
    input
    |> Array.toList
    |> List.fold (fun acc item -> (matchInput item)::acc) []
    |> List.rev

let processActions (input:Action list) =
    input
    |> List.fold (fun acc item ->
        let (items, curr) = acc
        let fs, current =
            match item with
            | ChangeDirectory Root -> Folder (Name = ["/"], Current = ["/"]), ["/"]
            | CreateFolder name -> Folder (Name = name::curr, Current = curr), curr
            | CreateFile (name, size) -> File (Name = name::curr, Size = size), curr
            | ChangeDirectory (Named name) -> Folder (Name = name::curr, Current = name::curr), name::curr
            | ChangeDirectory Parent -> Folder (Name = curr.Tail, Current = curr.Tail), curr.Tail
            | _ -> NotRequired, curr
        fs::items, current) ([], [])
    |> fun (x, _) -> x
    |> List.rev

let buildSubLists (input:string list) =
    let rec loop acc rem =
        match rem with
        | [] -> acc
        | items -> loop (items::acc) items.Tail
    loop [] input

let getFolders (input:FileSys list) =
    input
    |> List.choose (fun item ->
        match item with
        | Folder (Name = name; Current = current) when name = current -> Some name
        | _ -> None
    )
    |> List.distinct

let getFiles (input:FileSys list) =
    input
    |> List.choose (fun item ->
        match item with
        | File (Name = head::tail; Size = size) -> Some (head, tail |> buildSubLists, size)
        | _ -> None
    )

let aggregateData (input:FileSys list) =
    let folders = getFolders input
    let files = getFiles input
    folders
    |> List.map (fun dir -> 
        let dirFiles = files |> List.filter (fun (_,x,_) -> x |> List.contains dir)
        (dir, dirFiles |> List.sumBy (fun (_,_,s) -> s))
    )

let processData (input:string[]) =
    input
    |> processInputFile
    |> processActions
    |> aggregateData

module Part1 =

    let result = 
        data 
        |> processData
        |> List.filter (fun (_,s) -> s < 100_000)
        |> List.sumBy (fun (_,s) -> s)

    // let assertPart1Sample = result = 95437

module Part2 =

    let result = 
        data 
        |> processData
        |> List.sortByDescending (fun (_,s) -> s)
        |> fun x -> x |> List.head, x |> List.tail |> List.sortBy (fun (_,s) -> s) 
        |> fun ((_,v),t) -> t |> List.find (fun (_,s) -> 70_000_000 - v + s >= 30_000_000)
        |> fun (_,v) -> v

    // let assertPart2Sample = result = 24933642
