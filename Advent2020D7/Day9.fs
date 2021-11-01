namespace Advent2020.Day9

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

    [<Literal>]
    let windowSize = 25

    let uniqueArrayPairs (firstArray : 'a array) (secondArray : 'a array) : ('a * 'a) array =
        let indexedFirstArray = firstArray |> Array.indexed
        let indexedSecondArray = secondArray |> Array.indexed
        let pairedArray = indexedFirstArray |> Array.allPairs indexedSecondArray
        pairedArray
        |> Array.where (fun t ->
            match t with 
            | first, second -> (fst first) <> (fst second)
            )
        |> Array.map (fun t -> 
            match t with
            | first, second -> ((snd first), (snd second))
            )

    let getWindows (inputInts : int64 list) : int64 list list = 
        inputInts
        |> List.windowed windowSize
        |> List.rev //Reverse to remove the head
        |> List.tail //Remove the head
        |> List.rev

    let sumsToCheck (inputInts : int64 list) : int64 list = 
        inputInts
        |> List.splitAt windowSize
        |> snd

    let findSumFromWindow (window : int64 list) (sumToFind : int64) : int64 option = 
        let arrayWindow = window |> Array.ofList
        let arrayOfSums = 
            arrayWindow
            |> uniqueArrayPairs arrayWindow
            |> Array.map (fun n -> 
                match n with
                | (x, y) -> (x, y, x+y)
                )
        let containsSum = 
            arrayOfSums
            |> Array.map (fun f ->
                match f with
                | _, _, sum -> sum
                )
            |> Array.contains sumToFind
        if containsSum then None else Some(sumToFind)

    let getFirstCrack (inputList : int64 list) : int64 = 
        let windows = inputList |> getWindows
        let sums = inputList |> sumsToCheck
        (windows, sums)
        ||> List.map2 findSumFromWindow
        |> List.find (fun opt -> opt.IsSome)
        |> Option.defaultValue 0L

    let parse (fileInput : string list) : int64 list =
        fileInput
        |> List.map Int64.Parse

    let run : unit = 
        let fileName = "Advent2020D9.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        //alternateMissingID initialState
        printfn "First Cracked Number: %i" (getFirstCrack initialState)