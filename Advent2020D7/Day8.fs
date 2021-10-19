﻿namespace Advent2020.Day8

open System

type operation =
    | ACC
    | JMP
    | NOP

type command =
    {
        argument: int;
        operation: operation;
    }

module private Day8Helpers = 
    type opStrings = 
        {
            opString: string;
            argString: string;
        }

    type accumCommand = 
        {
            accumulator: int;
            commLine: int;
        }

module Option =
    let (>>=) r f = Option.bind f r
    let rtn v     = Some v
     
    let traverseList f ls = 
        let folder head tail = f head >>= (fun h -> tail >>= (fun t -> h::t |> rtn))
        List.foldBack folder ls (rtn List.empty)
    let sequenceList ls = traverseList id ls
    // val traverseList : ('a -> 'b option) -> 'a list -> 'b list option
    // val sequenceList : 'a option list -> 'a list option

module Main =
    let opParse (textList : string list) : command list option =
        let parser (line : string) : command option = 
            let splitLine = line.Split(' ')
            let splitArgs (entryLine : string[]) : Day8Helpers.opStrings option = 
                match entryLine with 
                | [|op; arg|] -> Some {opString = op; argString = arg}
                | _ -> None
            let argConvert (args: Day8Helpers.opStrings) : command option = 
                let oper : operation option = 
                    match args.opString.ToUpper () with
                    | "ACC" -> Some operation.ACC
                    | "JMP" -> Some operation.JMP
                    | "NOP" -> Some operation.NOP
                    | _ -> None
                let arg : int option = 
                    match System.Int32.TryParse args.argString with
                    | true,int -> Some int
                    | _ -> None
                let commandCreator (operate : operation option) (argumen : int option) : command option =
                    match (operate, argumen) with 
                    | Some x, Some y -> Some {operation = x; argument = y}
                    | _ -> None
                commandCreator oper arg
            splitLine
            |> splitArgs
            |> Option.bind argConvert
        let commandOptionToInvert (stringList : string list) = 
            stringList
            |> List.map parser
        Option.sequenceList (commandOptionToInvert textList)

    let numberedCommands (commandList : command list option) = 
        match commandList with
        | Some x -> 
            Some (x |> List.mapi (fun x y -> (x, y)))
        | None -> None

    //Main function should be a recursive function which passes an accumulating DICTIONARY, where on each iteration you add an element to the list which has the current location / command run
    //That dictionary will be indexed by line number run from, and if you hit the same element again then it will all return all the way up to the top with the accumulated list.

    //Use Option.map to use this function
    let getLoopAccum (numsCommands : (int * command) list) : int = 
        let commandsMapInitForAccum = (0, Map.empty<int, command>) //Integer is initial accumulator value, Map is the accumulating map which will record commands
        let commandMap = Map.ofList numsCommands //Create a map of the list of commands so they can be searched through
        let initCommand = 0 //The first command to run in the program
        
        let rec runProg (numCommandList : Map<int, command>) (commandMapAccum : (int * Map<int, command>)) (nextCommandToRun : int) = 
            let runCommand (command : command) (accumCommand : Day8Helpers.accumCommand) : Day8Helpers.accumCommand = 
                match command.operation with 
                | operation.ACC -> {accumulator = accumCommand.accumulator + command.argument; commLine = accumCommand.commLine + 1}
                | operation.JMP -> {accumulator = accumCommand.accumulator; commLine = accumCommand.commLine + command.argument}
                | operation.NOP -> {accumulator = accumCommand.accumulator; commLine = accumCommand.commLine + 1}
            let currentAccumCommand : Day8Helpers.accumCommand = {accumulator = fst commandMapAccum; commLine = nextCommandToRun}
            let oldMapRecord = snd commandMapAccum
            match oldMapRecord.ContainsKey nextCommandToRun with 
            | true -> Some (currentAccumCommand.accumulator, oldMapRecord)
            | false ->
                let nextCommand = numCommandList.TryFind nextCommandToRun
                match nextCommand with
                | None -> Some (currentAccumCommand.accumulator, oldMapRecord)
                | Some nextComm -> 
                    let newMapRecord = oldMapRecord.Add (nextCommandToRun, nextComm)
                    let newAccumInfo = runCommand nextComm currentAccumCommand
                    runProg (numCommandList) (newAccumInfo.accumulator, newMapRecord) newAccumInfo.commLine
        
        let accumAndRunCommands = runProg commandMap commandsMapInitForAccum initCommand

        match accumAndRunCommands with 
        | None -> 0
        | Some x ->
            let mapOfCommands = snd x
            let runCommands = Map.toList mapOfCommands
            printfn "%A" runCommands
            fst x

        
    let mainProg (fileName:string) =
        Advent2020.File.listedLines fileName
        |> opParse
        |> numberedCommands
        |> Option.map getLoopAccum

    let run : unit = 
        let fileName = "Advent2020D8.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let commandList = opParse fileInput

        
        //match numberedCommands commandList with 
        //| Some y -> y |> List.iter (fun x -> printfn "%s" (x.ToString()) )
        //| None -> printfn "%s" "Failed to parse"

        match mainProg fileName with
        | None -> printfn "Didn't work"
        | Some accum -> printfn "Final accumulator value: %i" accum

        //f# stinky poopy

        ()