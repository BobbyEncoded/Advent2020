namespace Advent2021.Day1

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq        

module Main =

    let parse (fileInput : string list) =
        fileInput
        |> List.map Int32.Parse

    let part1 (initInts : int list) = 
        let windows = initInts |> List.windowed 2
        let convertWindowToPlus (inputList : int list) = 
            if inputList.[1] > inputList.[0] then 1 else 0
        windows
        |> List.map convertWindowToPlus
        |> List.sum

    let run : unit = 
        let fileName = "Advent2021D1.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let part1ans = initialState |> part1

        printfn "%i" part1ans