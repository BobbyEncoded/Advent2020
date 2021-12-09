namespace Advent2021.Day5

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

type Point =
    {
        X : int
        Y : int
    }

module Main =

    let parse (fileInput : string list) =
        let coordsSplit (inputString : string) = 
            let coords = inputString.Split(" -> ")
            let firstCoords = coords.[0].Split(",")
            let secondCoords = coords.[1].Split(",")
            let firstX = firstCoords.[0] |> Int32.Parse
            let firstY = firstCoords.[1] |> Int32.Parse
            let secondX = secondCoords.[0] |> Int32.Parse
            let secondY = secondCoords.[1] |> Int32.Parse
            ({X = firstX; Y = firstY}, {X = secondX; Y = secondY})
        fileInput |> List.map coordsSplit

    let part1MapLine (inputMapPoints : Point * Point list) =
        let initialMapOfPointsAndCounts = Map.empty<Point, int>
        let drawLine (drawnPoints : Map<Point, int>) (mapPointToDraw : Point * Point) =
            let startPoint, endPoint = mapPointToDraw

    let run : unit = 
        let fileName = "Advent2021D5.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        
        printfn "Test"