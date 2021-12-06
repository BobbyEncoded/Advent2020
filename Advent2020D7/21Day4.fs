namespace Advent2021.Day4

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

type BingoBoardNum =
    {
        value : uint8
        called : bool
    }

module Main =

    let parse (fileInput : string list) =
        let splitStrings = 
            fileInput
            |> General.groupedStrings
        let calledNumbers = splitStrings |> List.head |> List.exactlyOne |> (fun x -> x.Split(',')) |> List.ofArray |> List.map uint8
        let bingoBoards = 
            splitStrings
            |> List.tail
            |> List.map (fun x ->
                x
                |> List.map (fun y ->
                    Regex.Matches(y,"\d+")
                    |> seq
                    |> Array.ofSeq
                    |> Array.map (fun m ->
                        {value = m.Value |> uint8; called = false}
                        )
                    )
                |> Array.ofList
                )
        calledNumbers, bingoBoards

    let makeNumCalled (numCalled : uint8) (bingoBoard : BingoBoardNum[][]) =
        bingoBoard
        |> Array.map (Array.map (fun bbn ->
                match (bbn.value = numCalled) with
                | false -> bbn
                | true -> {bbn with called = true}
                )
            )

    let checkForWinningBoard (bingoBoard : BingoBoardNum[][]) = 
        let calledPositions = bingoBoard |> Array.map (Array.map (fun bbn -> bbn.called))
        let checkRows = calledPositions |> Array.map (fun l -> l |> Array.forall id) |> Array.exists id
        let checkCols = calledPositions |> Array.transpose |> Array.map (fun l -> l |> Array.forall id) |> Array.exists id
        //let checkDiag1 = calledPositions[0][0] && calledPositions[1][1] && calledPositions[2][2] && calledPositions[3][3] && calledPositions[4][4]
        //let checkDiag2 = calledPositions[0][4] && calledPositions[1][3] && calledPositions[2][2] && calledPositions[3][1] && calledPositions[4][0]

        checkRows || checkCols// || checkDiag1 || checkDiag2

    let sumUncheckedNums (bingoBoard : BingoBoardNum[][]) = 
        bingoBoard |> Array.map (fun x -> x |> Array.map (fun bbn -> if bbn.called then 0UL else (bbn.value |> uint64)) |> Array.sum) |> Array.sum

    let rec Part1runBingo (numsToCall : uint8 list) (bingoBoards : BingoBoardNum[][] list) =
        let currentNum = numsToCall |> List.head
        let nextNums = numsToCall |> List.tail
        let callThisNum = makeNumCalled currentNum

        let calledNumOnBoards =
            bingoBoards
            |> List.map callThisNum

        let checkForWinningboards =
            calledNumOnBoards
            |> List.map (fun bb -> (bb, checkForWinningBoard bb))

        let foundWinningBoard = checkForWinningboards |> List.tryFind (fun (_,b) -> b)

        match foundWinningBoard with
        | None -> Part1runBingo nextNums calledNumOnBoards
        | Some wb -> (sumUncheckedNums (fst wb)) * (uint64 currentNum)

    let rec Part2RunBingo (numsToCall : uint8 list) (bingoBoards : BingoBoardNum[][] list) =
        let currentNum = numsToCall |> List.head
        let nextNums = numsToCall |> List.tail
        let callThisNum = makeNumCalled currentNum

        let calledNumOnBoards =
            bingoBoards
            |> List.map callThisNum

        let checkForWinningBoards =
            calledNumOnBoards
            |> List.map (fun bb -> (bb, checkForWinningBoard bb))

        let filterWonBoards =
            checkForWinningBoards
            |> List.filter (fun (_, b) -> not b)
            |> List.map fst

        let loneBoard = if ((filterWonBoards |> List.length) > 0) then filterWonBoards else (checkForWinningBoards |> List.map fst)

        match (loneBoard |> List.tryExactlyOne) with
        | None -> Part2RunBingo nextNums filterWonBoards
        | Some wb -> if (checkForWinningBoard wb) then (sumUncheckedNums wb) * (uint64 currentNum) else Part2RunBingo nextNums filterWonBoards

    let run : unit = 
        let fileName = "Advent2021D4.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let calledNums, bingoBoards = parse fileInput

        printfn "Part 1: %i" (Part1runBingo calledNums bingoBoards)
        printfn "Part 2: %i" (Part2RunBingo calledNums bingoBoards)