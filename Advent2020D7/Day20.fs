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
            let firstTileNeighbors = {topNeighbor = None; rightNeighbor = None; botNeighbor = None; leftNeighbor = None}
            {tileNum = tileID; orientation = Orientation.Normal; posX = 0; posY = 0; currentPixels = inputTileData; neighbors = firstTileNeighbors}
        let firstTileInGrid = [firstTile |> convertFirstTileToTileInGrid]

        let rec findConnectingTiles (tilesInGrid : TileInGrid list) = 
            let tilesRemainingToFind = inputTileMap.Count - tilesInGrid.Length
            let tilesRemainingInMap = 
                //Can update this so that the map stays recursive and we don't have to remove everything every time we go through the rec func.
                (inputTileMap, tilesInGrid) ||> List.fold (fun accMap tile -> accMap |> Map.remove tile.tileNum)
            //Stop condition
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

                    //Search for tiles for each edge
                    let tileMatchingFunction (searchForTile : bool) (edgesToMatchFunction : Edges -> Edges -> bool) = 
                        match searchForTile with
                        | false -> None
                        | true ->
                            let foundTile = 
                                tilesRemainingInMap
                                |> Map.map (fun mapIndex mapTile ->
                                    let matchedTile = 
                                        [Orientation.Normal, mapTile; Orientation.Rot90, mapTile |> TileRotations.rot90; Orientation.Rot180, mapTile |> TileRotations.rot180; Orientation.Rot270, mapTile |> TileRotations.rot270]
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
                                |> List.tryFind (fun (_, foundTile) ->
                                    match foundTile with
                                    | None -> false
                                    | Some _ -> true
                                    )
                                |> Option.bind snd
                            foundTile
                    let convertTileMatchingFunction (dxy : int * int) (inputTileMatch : (int * Orientation * bool[,])) = 
                        match inputTileMatch with |id, orientation, data ->
                        match dxy with |dx, dy ->
                            let neighbors = {topNeighbor = None; rightNeighbor = None; botNeighbor = None; leftNeighbor = None}
                            {tileNum = id; orientation = orientation; currentPixels = data; posX = tileToSearchForNeighbors.posX + dx; posY = tileToSearchForNeighbors.posY + dy; neighbors = neighbors}
                    //This function tries to find a tile, and returns data about the found tile if it finds one.
                    let topNeighborSearch = 
                        let searchForTile = searchForTopNeighbor
                        let topEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                            thisTileEdges.TopEdge = thatTileEdges.BotEdge
                        tileMatchingFunction searchForTile topEdgeEquivalenceFunction
                        |> Option.map (convertTileMatchingFunction (0,1))
                    let rightNeighborSearch = 
                        let searchForTile = searchForRightNeighbor
                        let rightEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                            thisTileEdges.RightEdge = thatTileEdges.LeftEdge
                        tileMatchingFunction searchForTile rightEdgeEquivalenceFunction
                        |> Option.map (convertTileMatchingFunction (1,0))
                    let botNeighborSearch = 
                        let searchForTile = searchForBotNeighbor
                        let botEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                            thisTileEdges.BotEdge = thatTileEdges.TopEdge
                        tileMatchingFunction searchForTile botEdgeEquivalenceFunction
                        |> Option.map (convertTileMatchingFunction (0,-1))
                    let leftNeighborSearch = 
                        let searchForTile = searchForLeftNeighbor
                        let leftEdgeEquivalenceFunction (thisTileEdges : Edges) (thatTileEdges : Edges) = 
                            thisTileEdges.LeftEdge = thatTileEdges.RightEdge
                        tileMatchingFunction searchForTile leftEdgeEquivalenceFunction
                        |> Option.map (convertTileMatchingFunction (-1,0))
                    [topNeighborSearch; rightNeighborSearch; botNeighborSearch; leftNeighborSearch]

                //Generate a tile in grid for each tile found, including its position.  Pass it back as a list of tiles to add to the main list.
                //Figure out how to handle duplicate tiles appearing in the master list.
                //Take that final list, and update all the tiles with their appropriate neighbors based on position.
                //Then recur the function.
                let tilesToAdd =
                    tilesInGrid
                    |> List.map searchATileForNeighbors
                    |> List.concat
                    |> List.choose id

                let getTileMapFromTileInGridList (tileInGridList : TileInGrid list) = 
                    tileInGridList |> List.map (fun x -> (x.posX, x.posY), x) |> Map.ofList

                let removeDuplicatesAndConglomerate = 
                    let tilesInGridMap = getTileMapFromTileInGridList tilesInGrid
                    (tilesInGridMap, tilesToAdd)
                    ||> List.fold (fun tileMap tileToAdd -> tileMap |> Map.add (tileToAdd.posX, tileToAdd.posY) tileToAdd)
                    |> Map.toList
                    |> List.map snd

                let updateNeighbors (tileInGridList : TileInGrid list) = 
                    let tileMap = getTileMapFromTileInGridList tilesInGrid
                    tileInGridList
                    |> List.map (fun x ->
                        let topNeighbor = tileMap |> Map.tryFind (x.posX, x.posY + 1) |> Option.map (fun x -> x.tileNum)
                        let rightNeighbor = tileMap |> Map.tryFind (x.posX + 1, x.posY) |> Option.map (fun x -> x.tileNum)
                        let botNeighbor = tileMap |> Map.tryFind (x.posX, x.posY - 1) |> Option.map (fun x -> x.tileNum)
                        let leftNeighbor = tileMap |> Map.tryFind (x.posX - 1, x.posY) |> Option.map (fun x -> x.tileNum)
                        let neighbors = {topNeighbor = topNeighbor; rightNeighbor = rightNeighbor; botNeighbor = botNeighbor; leftNeighbor = leftNeighbor}
                        {tileNum = x.tileNum; orientation = x.orientation; posX = x.posX; posY = x.posY; currentPixels = x.currentPixels; neighbors = neighbors}
                        )

                let newTiles = updateNeighbors removeDuplicatesAndConglomerate

                findConnectingTiles newTiles

        let matchedTiles = findConnectingTiles firstTileInGrid
        printfn "%A" matchedTiles


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

        findCornerTiles initialState

        printfn "Test"