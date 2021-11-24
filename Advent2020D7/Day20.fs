namespace Advent2020.Day20

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq        

module Main =

    let parse (fileInput : string list) =
        let tilesAndNames = 
            fileInput
            |> General.groupedStrings
        tilesAndNames
        |> List.map (fun tileAndName ->
            let tileIDStrings = tileAndName |> List.head
            let tileStrings = tileAndName |> List.tail

            let tileID = Regex.Match(tileIDStrings, @"\d+").Value |> Int32.Parse

            let tileChars = 
                tileStrings
                |> Array.ofList
                |> Array.map (fun s -> s.ToCharArray())

            let TileChars2D : char[,] = 
                let initializer (x : int) (y : int) = 
                    tileChars[y][x] //This makes x the first column and y the second column
                Array2D.init 10 10 initializer

                
            let boolArray = 
                TileChars2D
                |> Array2D.map (fun (c : char) ->
                    match c with
                    | '#' -> true
                    | '.' -> false
                    | _ -> raise (General.Unresolvable("Non pixel characters found in image strings."))
                    )

            (tileID, boolArray)
            )
        |> Map.ofList

        

    let run : unit = 
        let fileName = "Advent2020D20Test.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Test"