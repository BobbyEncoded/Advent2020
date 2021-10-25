namespace Advent2020.Day17

open System

type state =
    | Active
    | Inactive

type coordinate =
    {
        x: int;
        y: int;
        z: int;
        w: int;
    }

module private Day17Helpers = 
    //This will output a set of all coordinates which touches a certain coordinate
    let findTouchingCoordinates (inputCoordinates : coordinate Set) (inputCoordinate : coordinate) : coordinate Set = 
        seq {
            for s = inputCoordinate.x-1 to inputCoordinate.x+1 do
                for t = inputCoordinate.y-1 to inputCoordinate.y+1 do
                    for u = inputCoordinate.z-1 to inputCoordinate.z+1 do
                        for v = inputCoordinate.w-1 to inputCoordinate.w+1 do
                            yield {x = s; y = t; z = u; w = v}
        }
        |> Set.ofSeq

module Option =
    let (>>=) r f = Option.bind f r
    let rtn v     = Some v
     
    let traverseList f ls = 
        let folder head tail = f head >>= (fun h -> tail >>= (fun t -> h::t |> rtn))
        List.foldBack folder ls (rtn List.empty)
    let sequenceList ls = traverseList id ls
    // val traverseList : ('a -> 'b option) -> 'a list -> 'b list option
    // val sequenceList : 'a option list -> 'a list option

module Main = 
    //Need a function which will take in the input set of points and output the new set of points
    let iteratedSet (inputSet : coordinate Set) : coordinate Set =
        //Need a function which will find all points that will need to be checked to see if their state will be updated.
        //This will be done by creating a Set of all points touching currently active points, aka coordinates.
        //We will add each point touched by an input coordinate to an output set.
        //Then we will take that output set and check which points are active from the input coordinates, and add those in as active points.
        let findPointsToCheck (inputCoordinates : coordinate Set) : Map<coordinate, state> =
            //Need a function which will take a single coordinate and output a seq of coordinates which touch that point.
            //That function will be findTouchingCoordinates, listed in the helper functions

            //Now we'll need to take our input set of coordinates, run this function on each coordinate to get a sequence of these coordinate sets to union, resulting in a set of coordinates
            let setOfCoordsToCheck = 
                inputCoordinates
                |> Seq.map (Day17Helpers.findTouchingCoordinates inputCoordinates)
                |> Set.unionMany
            //Now we will need to take this set of coordinates, and make a map with the set as the Key values and set the state of all coordinates from the input state to active and the remaining states will be inactive
            //First we'll make the map of coordinates, with them all being inactive
            let allInactiveMapFromSetOfCoords (setOfCoords : coordinate Set) : Map<coordinate, state> = 
                setOfCoords
                |> Set.toSeq
                |> Seq.map (fun n -> (n, state.Inactive))
                |> Map.ofSeq
            //Now we need a function which will take the map of coordinates, and find all points on it and make them active.
            let updateMapWithActiveCoords (activeCoords : coordinate Set) (inactiveCoordsMap : Map<coordinate, state>) : Map<coordinate, state> = 
                //Create a mutable dictionary so we can edit things from the map
                let mutable inactiveCoordDict : System.Collections.Generic.Dictionary<coordinate, state> = 
                    let iCoordDict = 
                        inactiveCoordsMap
                        |> Map.toSeq
                        |> dict
                    new System.Collections.Generic.Dictionary<coordinate, state>(iCoordDict)
                //Edit the elements to be active wherever we find an entry
                for i in activeCoords do
                    inactiveCoordDict.[i] <- state.Active
                //Return with a map of the elements from the dictionary
                inactiveCoordDict
                |> Seq.map (|KeyValue|)
                |> Map.ofSeq
            //Now we have a function which outputs a map of active coordinates.  We just need to run the functions together.
            setOfCoordsToCheck
            |> allInactiveMapFromSetOfCoords
            |> updateMapWithActiveCoords inputCoordinates
        //We currently have a function which will tell us which points need to be checked to be updated.
        //Now we need to iterate through all the coordinates, and check each coordinates neighbor to see if the point should be/become active or inactive.
        //We can start with the function which will check a single point's neighbors to see what the new state of the point should be.
        let updateStateOfPoint (activeCoords : coordinate Set) (inputCoord : coordinate) (inputState : state) : state = 
            //First we need a function to count the number of points touching a single coordinate.
            //We will need a function which tells us which points touch our coordinate.
            //We can do that with the findTouchingCoordinates function, listed in the helpers.
            let pointsTouching : coordinate Set =
                Day17Helpers.findTouchingCoordinates activeCoords inputCoord
                |> Set.remove inputCoord //We also need to remove the central point for later reasons
            //Then we will need a function which checks all the points touching our coordinate and finds whether they are active or not.  For each one active, we will need to increase a counter.
            let countOfPointsTouching = 
                pointsTouching
                |> Set.filter (fun x -> activeCoords.Contains x)
                |> Set.count
            //Now we need to output the new state based on the current state and count of touching active cells
            match (inputState, countOfPointsTouching) with 
            | (state.Active, 2) | (state.Active, 3) -> state.Active
            | (state.Active, _) -> state.Inactive
            | (state.Inactive, 3) -> state.Active
            | (state.Inactive, _) -> state.Inactive
        let thisUpdateStateOfPoint = updateStateOfPoint inputSet
        //We can get a set of points to check, and can check each point to see what its new state is.
        inputSet
        |> findPointsToCheck //Get a list of all points to check
        |> Map.map thisUpdateStateOfPoint //Check each point's neighbors to see what the new state should be
        |> Map.filter( fun _ stateToCheck -> stateToCheck.Equals(state.Active)) //Trim out each state which isn't active
        |> Map.toSeq //Convert the map to a sequence so we can get the keys out and convert to a set
        |> Seq.map fst //Get only the key from the map of each element in the sequence
        |> Set.ofSeq //Convert the sequence to a set of keys

    //Now we need to build an initial set of states from our provided input file
    let parse (textList : string list) : coordinate Set =
        //We first need a function to take a string and output an array of valid coordinates
        let coordinateMaker (lineNum : int) (initString : string) : coordinate array = 
            let pointHasState (initPoint : coordinate * state) : bool = 
                let initState = snd initPoint
                initState.Equals(state.Active)
            initString.ToCharArray ()
            |> Array.mapi (fun i character -> if character = '#' then ({x = i; y = lineNum; z = 0; w = 0}, state.Active) else ({x = i; y = lineNum; z = 0; w = 0}, state.Inactive))
            |> Array.filter pointHasState
            |> Array.map fst
        //Then we will need to run this function through a map to make the coordinates, get the sets out, and union all the sets.
        textList
        |> List.mapi coordinateMaker
        |> List.map Set.ofArray
        |> Set.unionMany

    //Last but not least we will need a function which counts all the active coordinates from our output set
    let countActiveCells (inputCoords : coordinate Set) : int = 
        inputCoords.Count

    let run : unit = 
        let fileName = "Advent2020D17.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let countOfActiveCells =
            let rec nest n f x = if n=0 then x else nest (n-1) f (f x)
            initialState
            |> nest 6 iteratedSet
            |> countActiveCells

        printfn "Count of Active Cells: %i" countOfActiveCells