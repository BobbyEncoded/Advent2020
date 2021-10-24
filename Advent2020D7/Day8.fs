namespace Advent2020.Day8

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

    let numberedCommands (commandList : command list) = 
        commandList
        |> List.mapi (fun x y -> (x, y))

    //Main function should be a recursive function which passes an accumulating DICTIONARY, where on each iteration you add an element to the list which has the current location / command run
    //That dictionary will be indexed by line number run from, and if you hit the same element again then it will all return all the way up to the top with the accumulated list.

    //Use Option.map to use this function
    let getLoopAccum (numsCommands : (int * command) list) : int * bool = 
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
            | true -> Some (currentAccumCommand.accumulator, oldMapRecord, false) //If we have hit a loop condition, then we need to return out with the current accumulator value and record of instructions, and say that we didn't terminate
            | false ->
                let nextCommand = numCommandList.TryFind nextCommandToRun
                match nextCommand with
                | None -> Some (currentAccumCommand.accumulator, oldMapRecord, true) //If the next command cannot be found (appears outside the list), we're done and should return the current accumulator and the old record of commands, and say that we terminated
                | Some nextComm -> 
                    let newMapRecord = oldMapRecord.Add (nextCommandToRun, nextComm)
                    let newAccumInfo = runCommand nextComm currentAccumCommand
                    runProg (numCommandList) (newAccumInfo.accumulator, newMapRecord) newAccumInfo.commLine
        
        let accumAndRunCommands = runProg commandMap commandsMapInitForAccum initCommand

        match accumAndRunCommands with 
        | None -> (0, false)
        | Some x ->
            match x with 
            |accum, commands, noLoop ->
                //commands
                //|> Map.toList
                //|> printfn "%A"
                (accum, noLoop)
        
    //This is the function which finds the value of the accumulator of the original equation
    let accumAtLoop (fileName:string) =
        Advent2020.File.listedLines fileName
        |> opParse
        |> Option.map numberedCommands
        |> Option.map getLoopAccum

    //This is the function which will find the value of the terminating function when jmp is swapped with nop or nop with jmp
    let accumAtFinish (fileName:string) =
        let originalListOfCommands = 
            Advent2020.File.listedLines fileName
            |> opParse
            //This will need to be piped into numberedCommands to create a numbered list, unless I am using the piped version

        //I will need some function which will take the original list of commands, and output a sequence of lists of commands, with each list having a jmp or nop operation altered into a nop or jmp operation, respectively.
        let alteredCommandLists (originalCommandList : command list) : (command list list) =
            //Create a function which will take a list of commands and output a list of commands with each having a single jmp operation turned into a nop operation
            let linesWithReplacedCommands (opToFind : operation) (opToReplace : operation) (originalCommandList : command list) = 
                let listOfLinesFound =
                    originalCommandList
                    |> List.map ( fun commandInstance -> commandInstance.operation.Equals(opToFind))  //Use List.item to replace the itemed object with the new one with the replacement

                let replaceAtIndex (indexToChange : int) = //Uses local opToReplace and originalCommandList variables from higher level, to be made more generic this needs to be added as a parameter to THIS function
                    originalCommandList
                    |> List.mapi (fun (index2) (listCommand: command) -> if index2 = indexToChange then {argument = listCommand.argument; operation = opToReplace} else listCommand)
                
                let replacementFunction (listOfReplacements : bool list) = 
                    listOfReplacements
                    |> List.mapi (fun (index) (replaceList : bool) -> if replaceList then Some(replaceAtIndex index) else None)

                let trimNonesAndReplaceSomesInList (listToTrim : 'a option list) : ('a list) = 
                    listToTrim
                    |> List.filter (fun x -> x.IsSome)
                    |> List.map (fun x -> x.Value)

                listOfLinesFound
                |> replacementFunction
                |> trimNonesAndReplaceSomesInList

            linesWithReplacedCommands operation.JMP operation.NOP originalCommandList
            |> List.append (linesWithReplacedCommands operation.NOP operation.JMP originalCommandList)

        //This command will get us the altered command lists
        let numberedAlteredCommandLists (listToAlterAndNumber : command list) =
            listToAlterAndNumber
            |> alteredCommandLists
            |> List.map numberedCommands

        //Now I will need a function which will take a list of command lists, and run our looping function on the list of command lists, and return to us the accumulator for the function which returns true for terminating
        let valueOfAccumWhichTerminates (listOfCommandListsToCheck : (int * command) list list) = 
                listOfCommandListsToCheck
                |> List.map getLoopAccum
                |> List.find (fun x -> snd(x))
                |> fst

        originalListOfCommands
        |> Option.map numberedAlteredCommandLists
        |> Option.map valueOfAccumWhichTerminates


    let run : unit = 
        let fileName = "Advent2020D8.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let commandList = opParse fileInput

        
        //match numberedCommands commandList with 
        //| Some y -> y |> List.iter (fun x -> printfn "%s" (x.ToString()) )
        //| None -> printfn "%s" "Failed to parse"

        match accumAtLoop fileName with
        | None -> printfn "Didn't work"
        | Some accum ->
            match accum with
            |accumValue, noLooped ->
                printfn "Final accumulator value: %i. Whether the program hit the end condition: %b" accumValue noLooped

        match accumAtFinish fileName with
        | None -> printfn "Didn't work"
        | Some accum -> printfn "Final accumulator value of terminating program: %i." accum

        //f# stinky poopy

        ()