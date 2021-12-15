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
        initInts
        |> List.windowed 2
        |> List.map (fun i -> if i.[1] > i.[0] then 1 else 0)
        |> List.sum

    let part2 (initInts : int list) = 
        initInts
        |> List.windowed 3
        |> List.map List.sum
        |> part1

    let run : unit = 
        let fileName = "Advent2021D1.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let part1ans = initialState |> part1
        let part2ans = initialState |> part2

        printfn "%i" part1ans
        printfn "%i" part2ans