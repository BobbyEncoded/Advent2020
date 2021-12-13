namespace Advent2021.Day6

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

module Main =

    let parse (fileInput : string list) =
        let initialMap = seq {0y .. 8y} |> Seq.map (fun v -> (v, 0UL)) |> Map.ofSeq
        let fishDays = 
            fileInput
            |> List.exactlyOne
            |> (fun s -> s.Split(','))
            |> Array.map int8
        (initialMap, fishDays)
        ||> Array.fold (fun counts fish ->
            counts
            |> Map.change fish (fun s -> match s with |None -> None |Some v -> Some (v + 1UL))
            )

    let runFish (targetDate : int) (countMap : Map<sbyte, uint64>) =
        let initialDay = 0
        let rec newDay (currentFish : Map<sbyte, uint64>) (currentDay : int) =
            let reachedTargetDate = currentDay >= targetDate
            match reachedTargetDate with
            | true -> currentFish
            | false ->
                let updatedFish =
                    let mappingFunction (key : sbyte) (_ : uint64) =
                        if key = 8y then currentFish.Item 0y
                        elif key = 6y then (currentFish.Item 7y) + (currentFish.Item 0y)
                        else currentFish.Item (key + 1y)
                    currentFish |> Map.map mappingFunction
                newDay updatedFish (currentDay + 1)
        newDay countMap initialDay

    let sumFish (countMap : Map<sbyte, uint64>) =
        (0UL, countMap)
        ||> Map.fold (fun counts _ numFish -> counts + numFish)
                        

    let run : unit = 
        let fileName = "Advent2021D6.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let finalFishPart1 = initialState |> runFish 80 |> sumFish
        let finalFishPart2 = initialState |> runFish 256 |> sumFish
        
        printfn "%i" finalFishPart2