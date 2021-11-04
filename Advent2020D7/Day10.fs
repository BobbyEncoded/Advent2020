namespace Advent2020.Day10

open System


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

    let createOrganizedList (rawInput : int list) : int list = 
        rawInput
        |> List.sort

    let addUserDevice (inputList : int list) : int list =
        let maxInList = inputList |> List.max
        inputList
        |> List.append [0; (maxInList + 3)]

    let listOfDifferences (inputList : int list) : int list = 
        let windowedList = inputList |> List.windowed 2
        let differenceInWindow (windowList : int list) : int = 
            let windowHead = windowList |> List.head
            let windowEnd = windowList |> List.last
            (windowEnd - windowHead) |> Math.Abs
        windowedList
        |> List.map differenceInWindow

    let countNumberOfDifferences (inputList : int list) : (int * int) list = 
        inputList
        |> List.countBy (fun l -> l)

    let multiplyMinMaxCount (inputKeysAndCounts : (int * int) list) : int = 
        let minCount = 
            inputKeysAndCounts
            |> List.minBy (fun i -> (fst i))
            |> snd
        let maxCount = 
            inputKeysAndCounts
            |> List.maxBy (fun i -> (fst i))
            |> snd
        minCount * maxCount

    let arrayLog (inputList : 'a list) : 'a list = 
        printfn "%A" inputList
        inputList

    let possibleAdapterCombinations (sourceList : int list) : int list list =
        //let startingAdapter = [(1, 0)]
        let maxSourceAdapter = sourceList |> List.max |> (+) 3
        let rec adapterCombinationPossibilities (currentAdapters : int list) : int list list =
            let findAdaptersWithin3 (maxAdapter : int) : int list =
                sourceList
                |> List.filter (fun p -> (p > maxAdapter) && (p <= maxAdapter + 3))
            let maxRecAdapter = 
                currentAdapters
                |> List.max
            let nextAdapters =
                maxRecAdapter
                |> findAdaptersWithin3
            match nextAdapters with 
            | [] -> if (maxRecAdapter = maxSourceAdapter) then (currentAdapters |> List.singleton) else (List<int>.Empty |> List.singleton)
            | nextAdapters ->
                nextAdapters
                |> List.map (fun i ->
                    i
                    |> List.singleton
                    |> List.append currentAdapters)
                |> List.map adapterCombinationPossibilities
                |> List.concat
        adapterCombinationPossibilities [0]

    let countArrangements (inputList : int list) : int = 
        inputList
        |> possibleAdapterCombinations
        |> List.length

    let mainRun (inputList : int list) : int = 
        inputList
        |> addUserDevice
        |> createOrganizedList
        |> listOfDifferences
        |> countNumberOfDifferences
        |> arrayLog
        |> multiplyMinMaxCount
        

    let parse (fileInput : string list) : int list =
        fileInput
        |> List.map Int32.Parse

    let run : unit = 
        let fileName = "Advent2020D10.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        //alternateMissingID initialState
        printfn "Counts Multiplied: %i" (countArrangements initialState)