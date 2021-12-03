namespace Advent2021.Day3

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

module Main =

    let parse (fileInput : string list) =
        fileInput
        |> List.map (fun x -> x.ToCharArray())
        |> List.map (fun x -> x |> Array.map (fun c -> match c with |'1' -> true; |'0' -> false; |_ -> raise (General.Unresolvable("Invalid Parse"))))

    let convertBitsToInt (t : System.Collections.BitArray) =
        let mutable intArray = 0 |> Array.singleton
        t.CopyTo(intArray, 0)
        intArray |> Array.exactlyOne

    let part1 (inputBools : bool array list) =
        let boolListsForTranspose = inputBools |> List.map List.ofArray |> List.transpose
        let bitArray = 
            boolListsForTranspose
            |> List.map (fun l ->
                let zeroOneCounts = (0,0)
                (zeroOneCounts, l)
                ||> List.fold (fun (zeroCount, oneCount) bool ->
                    if bool then (zeroCount, oneCount + 1) else (zeroCount + 1, oneCount)
                    )
                |> (fun t -> match t with | (zeroCount, oneCount) -> if (zeroCount > oneCount) then false else true)
                )
            |> Array.ofList
            |> Array.rev
            |> System.Collections.BitArray
        (convertBitsToInt bitArray) * (convertBitsToInt (bitArray.Not()))

    let part2 (inputBools : bool array list) = 
        let testForMostCommon (zeroCount : int) (oneCount : int) =
            if (zeroCount > oneCount) then false else true
        let testForLeastCommon (zeroCount : int) (oneCount : int) =
            if (zeroCount <= oneCount) then false else true
        let rec getOxygen (digitCount : int -> int -> bool) (lastBools : bool array list) (indexToCheck : int) = 
            let digitsToSeeMostCommon = lastBools |> List.map (fun b -> b.[indexToCheck])
            let zeroOneCounts = (0,0)
            let mostCommonDigit = 
                (zeroOneCounts, digitsToSeeMostCommon)
                ||> List.fold (fun (zeroCount, oneCount) bool ->
                    if bool then (zeroCount, oneCount + 1) else (zeroCount + 1, oneCount)
                    )
                |> (fun t -> match t with | (zeroCount, oneCount) -> digitCount zeroCount oneCount)
            let filteredList = 
                lastBools |> List.filter (fun ba -> ba.[indexToCheck] = mostCommonDigit)
            match filteredList |> List.length with
            | 1 -> filteredList |> List.exactlyOne |> Array.rev |> System.Collections.BitArray |> convertBitsToInt
            | _ -> getOxygen digitCount filteredList (indexToCheck+1)
        let firstNum = (getOxygen testForMostCommon inputBools 0)
        let secondNum = (getOxygen testForLeastCommon inputBools 0)
        firstNum * secondNum        

    let run : unit = 
        let fileName = "Advent2021D3.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let part1ans = initialState |> part1
        let part2ans = initialState |> part2

        printfn "%i" part1ans
        printfn "%i" part2ans