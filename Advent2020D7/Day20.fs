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

type Neighbors =
    {
        topNeighbor : int option
        rightNeighbor : int option
        botNeighbor : int option
        leftNeighbor : int option
    }
type Orientation =
    | Normal
    | Rot90
    | Rot180
    | Rot270
//Describes a tile in the major grid.
type TileInGrid =
    {
        tileNum : int
        orientation: Orientation
        posX : int
        posY : int //Greater y is down
        currentPixels: bool[,] //Pixels of tile in rotated state
        neighbors: Neighbors
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

    let findCornerTiles (inputTileMap : Map<int,bool[,]>) = 
        let firstTile = inputTileMap |> Map.toSeq |> Seq.head
        let convertFirstTileToTileInGrid (tileID : int, inputTileData : bool[,]) = 
            let firstTileNeighbors = Neighbors{topNeighbor = None; rightNeighbor = None; botNeighbor = None; leftNeighbor = None}
            {tileNum = tileID; orientation = Orientation.Normal; posX = 0; posY = 0; currentPixels = inputTileData; neighbors = firstTileNeighbors}
        let firstTileInGrid = [firstTile |> convertFirstTileToTileInGrid]

        let rec findConnectingTiles (tilesInGrid : TileInGrid list) = 
            let tilesRemainingToFind = inputTileMap.Count - tilesInGrid.Length
            let tilesRemainingInMap = 
                //Can update this so that the map stays recursive and we don't have to remove everything every time we go through the rec func.
                (inputTileMap, tilesInGrid) ||> List.fold (fun accMap tile -> accMap |> Map.remove tile.tileNum)
            match tilesRemainingToFind with
            | 0 -> tilesInGrid
            | _ ->
                let searchATileForNeighbors (tileToSearchForNeighbors : TileInGrid) = 
                    let shouldSearchForNeighbor (inputNeighbor : 'T option) =
                        match inputNeighbor with
                        | None -> true
                        | Some _ -> false
                    let searchForTopNeighbor = shouldSearchForNeighbor tileToSearchForNeighbors.neighbors.topNeighbor
                    let searchForRightNeighbor = shouldSearchForNeighbor tileToSearchForNeighbors.neighbors.rightNeighbor
                    let searchForBotNeighbor = shouldSearchForNeighbor tileToSearchForNeighbors.neighbors.botNeighbor
                    let searchForLeftNeighbor = shouldSearchForNeighbor tileToSearchForNeighbors.neighbors.leftNeighbor

                    //PrepForEdgeSearches


                    let tileMatchingFunction (searchForTile : bool) (edgesToMatchFunction : Edges -> Edges -> bool) = 
                        match searchForTile with
                        | false -> None
                        | true ->
                            let foundTile = 
                                tilesRemainingInMap
                                |> Map.map (fun mapIndex mapTile ->
                                    let matchedTile = 
                                        [Orientation.Normal, mapTile; Orientation.Rot90, mapTile |> Rot90; Orientation.Rot180 mapTile |> Rot180; Orientation.Rot270, mapTile |> Rot270]
                                        |> List.map (fun (orientation, tile) ->
                                            let thisTileEdges = TileRotations.getEdgesFromTile tileToSearchForNeighbors.currentPixels
                                            let thatTileEdges = TileRotations.getEdgesFromTile tile
                                            let areSameSide = edgesToMatchFunction thisTileEdges thatTileEdges
                                            match areSameSide with
                                            | false -> None
                                            | true -> Some(orientation, tile)
                                            )
                                        |> List.tryFind (fun matchingTile ->
                                            match matchingTile with
                                            | None -> false
                                            | Some _ -> true
                                            )
                                    match matchedTile with
                                    | None -> None
                                    | Some foundTile ->
                                        match foundTile with
                                        | None -> None
                                        | Some (foundTileOrientation, foundTileData) -> Some (mapIndex, foundTileOrientation, foundTileData)
                                    )
                                |> Map.toList
                                |> List.find (fun (_, foundTile) ->
                                    match foundTile with
                                    | None -> false
                                    | Some _ -> true
                                    )
                                |> snd
                            foundTile

                    //This function tries to find a tile, and returns data about the find tile if it finds one.
                    let topNeighborSearch = 
                        let searchForTile = searchForTopNeighbor
                        let topEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                            thisTileEdges.TopEdge = thatTileEdges.BotEdge
                        tileMatchingFunction searchForTile topEdgeEquivalenceFunction
                        (*
                        match searchForTopNeighbor with
                        | false -> None
                        | true ->
                            let topEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                                thisTileEdges.TopEdge = thatTileEdges.BotEdge
                            let foundTile = 
                                tilesRemainingInMap
                                |> Map.map (fun mapIndex mapTile ->
                                    let matchedTile = 
                                        [Orientation.Normal, mapTile; Orientation.Rot90, mapTile |> Rot90; Orientation.Rot180 mapTile |> Rot180; Orientation.Rot270, mapTile |> Rot270]
                                        |> List.map (fun (orientation, tile) ->
                                            let thisTileEdges = TileRotations.getEdgesFromTile tileToSearchForNeighbors.currentPixels
                                            let thatTileEdges = TileRotations.getEdgesFromTile tile
                                            let areSameSide = topEdgeEquivalenceFunction thisTileEdges thatTileEdges
                                            match areSameSide with
                                            | false -> None
                                            | true -> Some(orientation, tile)
                                            )
                                        |> List.tryFind (fun matchingTile ->
                                            match matchingTile with
                                            | None -> false
                                            | Some _ -> true
                                            )
                                    match matchedTile with
                                    | None -> None
                                    | Some foundTile ->
                                        match foundTile with
                                        | None -> None
                                        | Some (foundTileOrientation, foundTileData) -> Some (mapIndex, foundTileOrientation, foundTileData)
                                    )
                                |> Map.toList
                                |> List.find (fun (_, foundTile) ->
                                    match foundTile with
                                    | None -> false
                                    | Some _ -> true
                                    )
                                |> snd
                            foundTile
                            *)

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