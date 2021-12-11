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

    let part1MapLine (inputMapPoints : (Point * Point) list) =
        let initialMapOfPointsAndCounts = Map.empty<Point, int>
        let drawLine (drawnPoints : Map<Point, int>) (mapPointToDraw : Point * Point) =
            let startPoint, endPoint = mapPointToDraw
            let AddPointsToMap (inputPoints : Point seq) =
                let foldPointsIntoMap (inputMap : Map<Point, int>) (pointToAdd : Point) =
                    let currentVal = inputMap |> Map.tryFind pointToAdd
                    match currentVal with
                    | Some _ -> inputMap |> Map.change pointToAdd (fun s -> match s with |None -> None |Some s -> Some (s+1))
                    | None -> inputMap |> Map.add pointToAdd 1
                (drawnPoints, inputPoints) ||> Seq.fold foldPointsIntoMap
            if startPoint.X = endPoint.X
            then
                let numbers = if startPoint.Y < endPoint.Y then seq {startPoint.Y .. endPoint.Y} else seq {startPoint.Y .. -1 .. endPoint.Y}
                let points = numbers |> Seq.map (fun yPoint -> {X = startPoint.X; Y = yPoint})
                AddPointsToMap points
            elif startPoint.Y = endPoint.Y
            then
                let numbers = if startPoint.X < endPoint.X then seq {startPoint.X .. endPoint.X} else seq {startPoint.X .. -1 .. endPoint.X}
                let points = numbers |> Seq.map (fun xPoint -> {X = xPoint; Y = startPoint.Y})
                AddPointsToMap points
            else
                //Return inputMapPoints for Part1 instead of this else statement
                let xDirection = (endPoint.X - startPoint.X) / abs(endPoint.X - startPoint.X)
                let yDirection = (endPoint.Y - startPoint.Y) / abs(endPoint.Y - startPoint.Y)
                let xSeq = seq {startPoint.X .. xDirection .. endPoint.X}
                let ySeq = seq {startPoint.Y .. yDirection .. endPoint.Y}
                let points = (xSeq, ySeq) ||> Seq.map2 (fun x y -> {X = x; Y = y})
                AddPointsToMap points
        (initialMapOfPointsAndCounts, inputMapPoints)
        ||> List.fold drawLine

    let countIntersects (inputMap : Map<Point, int>) =
        let initialCounts = 0
        (initialCounts, inputMap)
        ||> Map.fold (fun c _ i -> if i > 1 then (c + 1) else c)

    let run : unit = 
        let fileName = "Advent2021D5.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        let intersectCount = initialState |> part1MapLine |> countIntersects
        
        printfn "%i" intersectCount