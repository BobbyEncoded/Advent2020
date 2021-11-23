namespace Advent2020.Day6

open System
open Advent2020.General

module Main =

    let handleAnswers (inputAnswers : char Set list list) (setArrangementFunc : seq<Set<char>> -> Set<char>) : int = 
        inputAnswers
        |> List.map (fun f ->
            f
            |> setArrangementFunc
            |> Set.count
            )
        |> List.sum

    let countUniqueQuestionsAnswer (inputAnswers : char Set list list) : int = 
        handleAnswers inputAnswers Set.unionMany

    let countQuestionsAnswerByAll (inputAnswers : char Set list list) : int = 
        handleAnswers inputAnswers Set.intersectMany

    let parse (fileInput : string list) : char Set list list =
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

        printfn "Sum: %i" (countQuestionsAnswerByAll initialState)