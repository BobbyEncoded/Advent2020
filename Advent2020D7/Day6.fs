namespace Advent2020.Day6

open System

type seat = 
    {
        row : int;
        col : int;
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

    let countUniqueQuestionsAnswer (inputAnswers : char Set list list) : int = 
        inputAnswers
        |> List.map (fun f ->
            f
            |> Set.unionMany
            |> Set.count
            )
        |> List.sum
        

    let parse (fileInput : string list) : char Set list list =
        let groupedStrings (rawInput : string list) : string list list = 
            let inputFolder (acc : string list list) (entryToAdd : string) : string list list =
                match entryToAdd with 
                | "" ->
                    let emptyStringList = List<string>.Empty //Note that this is a property with capital E of List<string>
                    let newEmptyStringList = List.singleton emptyStringList
                    List.append acc newEmptyStringList
                | entryToAdd ->
                    let lastStringListSplitIndex = 
                        ((acc |> List.length) - 1)
                    let front, last = acc |> List.splitAt lastStringListSplitIndex
                    let updateFinalString (stringListListToReplace : string list list) : string list list = 
                        stringListListToReplace
                        |> List.head
                        |> List.append (List.singleton entryToAdd)
                        |> List.singleton
                    last
                    |> updateFinalString
                    |> List.append front
            let initialAcc : string list list =
                List.empty<string>  //While this is a function which takes a string type, it does the same thing.
                |> List.singleton
            (initialAcc, rawInput)
            ||> List.fold inputFolder
        fileInput
        |> groupedStrings
        |> List.map (fun f ->
            f
            |> List.map (fun f ->
                f.ToCharArray()
                |> Set.ofArray
                )
            )

    

    let run : unit = 
        let fileName = "Advent2020D6.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        //alternateMissingID initialState
        printfn "Sum: %i" (countUniqueQuestionsAnswer initialState)