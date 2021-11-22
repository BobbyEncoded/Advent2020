namespace Advent2020.Day15

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open HPCsharp

module Main =

    [<Literal>]
    let finalCount = 30000000UL

    let initialListTest = [0UL;3UL;6UL]
    let initialList = [1UL;0UL;15UL;2UL;10UL;13UL]

    let parse (init : uint64 list) = 
        //Convert the list into a map with keys based on the value inside and values which contain the last time the key was referenced.
        //These values will be in the form of a Set of up to 2 elements.  Sets are already sorted.
        let indexed = init |> List.indexed |> List.map (fun x -> match x with |x, y -> uint64 x+1UL, uint64 y)
        let initialMap = 
            let (indexList, valList) = indexed |> List.unzip
            let mapPrepList = (valList, indexList) ||> List.zip
            mapPrepList
            |> List.map (fun x -> match x with |x, y -> (x, y |> Set.singleton))
            |> Map.ofList
        initialMap, uint64 initialMap.Count, (init |> List.last)

    let findValAtTurn (initialMap : Map<uint64, Set<uint64>>, initCount : uint64, initLastVal : uint64) (targetCount : uint64) = 

        let rec findNextTurn (currentMap : Map<uint64, Set<uint64>>) (lastCount : uint64) (lastVal : uint64)  = 
            let newCount = lastCount + 1UL
            let newSet = newCount |> Set.singleton
            match (lastCount >= targetCount) with
            | true -> lastVal
            | false ->
                match (currentMap.TryFind lastVal) with
                | None ->
                    let newMap = currentMap.Add(lastVal, newSet)
                    let newVal = 0UL
                    findNextTurn newMap newCount newVal
                | Some indices ->
                    let updateMap newVal = currentMap |> Map.change newVal (fun valSet ->
                        match valSet with 
                        | None -> Some(newSet)
                        | Some valSet -> Set.maxElement (valSet) |> Set.singleton |> Set.union newSet |> Some
                        )
                    match indices.Count with
                    | 1 ->                        
                        let newVal = 0UL
                        let newMap = updateMap newVal
                        findNextTurn newMap newCount newVal
                    | _ ->
                        let maxVal = (indices |> Set.maxElement)
                        let minVal = (indices |> Set.minElement)
                        let newVal = maxVal - minVal
                        let newMap = updateMap newVal
                        findNextTurn newMap newCount newVal

        findNextTurn initialMap initCount initLastVal

    let run : unit = 
        let initialState = parse initialList
        let finalState = findValAtTurn initialState finalCount
        printfn "%i" finalState