
// Renaming files

open System
open System.IO

[<Literal>]
let Folder = """P:\CloudStaton\PhotoVideo\SOURCE\Thailand\2019 - 04 - Koh Lipe"""
//let Folder = """P:\CloudStaton\PhotoVideo\SOURCE\Thailand\test"""

type ResultRename =
| Success of string
| Error of string


let combineOpt ((x,y): 'a * 'b option) =
    match y with
    | Some b -> Some (x,b)
    | None   -> None

let getListFiles (filters: (string -> bool) list) (folder: string) =
    let rec filtering listFilters files =
        match listFilters with
        | [] -> files
        | h::t -> filtering t (Seq.filter h files)
    filtering filters (Directory.GetFiles(folder))
    

let printListFiles (filters: (string -> bool) list) (folder: string) =
    getListFiles filters folder
    |> Seq.iter (printfn "%s")

let createNewName renamingRule file =
    let path =
        Path.GetDirectoryName(file) + "\\"
    let oldName =
        Path.GetFileName(file)
    let newName = renamingRule oldName
    
    match newName with
    | Some n -> Some(path + n)
    | None   -> None


let fsFileMove (oldFile, newFile) =
    try
        File.Move(oldFile, newFile)
        printfn "SUCCESS! %s => %s" (Path.GetFileName(oldFile)) (Path.GetFileName(newFile))
        ResultRename.Success newFile
    with
    | :? System.IO.IOException as ex -> 
        printfn "Error IO - File may already exists or Name is empty: %s" ex.Message
        ResultRename.Error ex.Message
    | :? System.ArgumentException as ex ->
        printfn "Error: %s" ex.Message
        ResultRename.Error ex.Message
    | :? System.UnauthorizedAccessException as ex ->
        printfn "Error IO - Unauthorized Access: %s" ex.Message
        ResultRename.Error ex.Message
    | _ ->
        printfn "ERROR not defined"
        ResultRename.Error ("Error not defined for file: " + newFile)




let rename (renamingRule: string -> string) file =
    file
    |> Path.GetFileName
    |> (fun old -> old, renamingRule old)


let printResult (res: ResultRename) =
    match res with
    | Success str ->
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "%s" str
    | Error str ->
        Console.ForegroundColor <- ConsoleColor.Red
        printfn "%s" str

    Console.ForegroundColor <- ConsoleColor.White

let printSummary (listRes: ResultRename list) =
    let trueFalse =
        function
        | Success _ -> true
        | Error _   -> false
    
    let success =
        listRes
        |> List.filter (fun res -> trueFalse res)
        |> List.length

    let failure =
        (List.length listRes) - success

    //listRes
    //|> Seq.iter printResult

    printfn "SUMMARY: \nTotal files to rename: %i" (List.length listRes)
    printfn "Success: %i" success 
    printfn "Failure: %i" failure



let renameBatch (filters: (string -> bool) list)
    (renamingRule: string -> string option) folder =
    Directory.GetFiles(folder)
    |> Seq.length
    |> (printfn "Total files in the folder: %i")

    let selected =
        getListFiles filters folder

    selected
    |> Seq.length
    |> (printfn "Total files selected/filtered: %i")
    
    selected
    |> Seq.map (fun file -> file, createNewName renamingRule file)
    |> Seq.choose combineOpt
    |> Seq.map fsFileMove
    |> Seq.toList
    |> printSummary

Directory.GetFiles(Folder)
|> Seq.iter (fun s -> (printfn "%s" (Path.GetDirectoryName(s))))

let ruleErr (str: string) =
    if (str.[..1] = "PA") then Some("") else None


let rule1 (str: string) =
    if (str.[..1] = "_P") then Some("P" + str.[2..]) else None

renameBatch []  rule1 Folder