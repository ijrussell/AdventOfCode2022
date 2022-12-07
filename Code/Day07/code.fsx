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
    Path.Combine(__SOURCE_DIRECTORY__, "sample-data.txt")
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
    let rec loop acc (rem:string list) =
        match rem with
        | [] -> acc |> List.rev
        | head::tail ->
            let action = matchInput head
            loop (action::acc) tail
    loop [] (input |> Array.toList)

let processActions (input:Action list) =
    let rec loop (acc:FileSys list) (curr:string list) (rem:Action list) =
        match rem with
        | [] -> acc |> List.rev
        | head::tail ->
            let fs, current =
                match head with
                | ChangeDirectory Root -> Folder (Name = ["/"], Current = ["/"]), ["/"]
                | CreateFolder name -> Folder (Name = name::curr, Current = curr), curr
                | CreateFile (name, size) -> File (Name = name::curr, Size = size), curr
                | ChangeDirectory (Named name) -> Folder (Name = name::curr, Current = name::curr), name::curr
                | ChangeDirectory Parent -> Folder (Name = curr.Tail, Current = curr.Tail), curr.Tail
                | _ -> NotRequired, curr
            loop (fs::acc) current tail
    loop [] [] input

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
    |> List.filter (fun (_,s) -> s < 100_000)
    |> List.sumBy (fun (_,s) -> s)

let result = 
    data 
    |> processInputFile
    |> processActions
    |> aggregateData
