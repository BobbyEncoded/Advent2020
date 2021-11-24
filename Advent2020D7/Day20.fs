namespace Advent2020.Day20

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

type Edges =
    {
        TopEdge : bool array
        RightEdge : bool array
        BotEdge : bool array
        LeftEdge : bool array
    }

module private TileRotations = 

    let getEdgesFromTile (inputTile : bool[,]) = 
        let leftEdge = inputTile[0, *]
        let topEdge = inputTile[*, 0]
        let rightEdge = inputTile[topEdge.Length - 1, *]
        let botEdge = inputTile[*, rightEdge.Length - 1]
        {TopEdge = topEdge; RightEdge = rightEdge; BotEdge = botEdge; LeftEdge = leftEdge}

    let getSideAndTopLength (inputTile : bool[,]) : int * int = 
        let topSize = inputTile[0, *] |> Array.length
        let sideSize = inputTile[*, 0] |> Array.length
        sideSize, topSize

    let rot90 (inputTile : bool[,]) : bool[,] =
        let sideLength, topLength = getSideAndTopLength inputTile
        Array2D.init sideLength topLength (fun x y -> inputTile[y, (sideLength-1)-x])

    let rot180 (inputTile : bool[,]) = 
        let sideLength, topLength = getSideAndTopLength inputTile
        Array2D.init sideLength topLength (fun x y -> inputTile[(topLength-1) - x, (sideLength-1) - y])

    let rot270 (inputTile : bool[,]) : bool[,] =
        let sideLength, topLength = getSideAndTopLength inputTile
        Array2D.init sideLength topLength (fun x y -> inputTile[(sideLength-1)-y, x])

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

        let sampleTile = 
            initialState
            |> Map.find 1171

        let rot90Tile = sampleTile |> TileRotations.rot90
        let rot180Tile = sampleTile |> TileRotations.rot180
        let rot270Tile = sampleTile |> TileRotations.rot270

        printfn "Test"