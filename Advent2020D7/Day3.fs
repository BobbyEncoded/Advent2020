namespace Advent2020.Day3

open System

type slopeLand = 
    {
        tree : bool;
    }

type slope = 
    {
        right: int;
        down: int;
    }

(*
module private Day1Helpers = 
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
*)

exception Unparsable of string

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

    let treesHit (inputMap : slopeLand seq list) (inputSlope : slope) : int =
        let downStep = inputSlope.down;
        let rightStep = inputSlope.right;
        //First we're going to shorten the list so we only care about our down steps, so now we just need to worry about how far right to go on each iteration
        let downStepMap (originalMap : slopeLand seq list) (downSlope : int) : slopeLand seq list = 
            let indexedMap = originalMap |> List.indexed
            let filter (indexedInput : int * obj) : bool = 
                let index = fst indexedInput
                let remainder = index % downSlope
                match remainder with 
                | 0 -> true
                | _ -> false
            let filteredList = List.filter filter
            indexedMap
            |> filteredList
            |> List.map (fun x -> snd x)
        let downSteppedMap = downStepMap inputMap downStep
        //Need a function which will return 1 or 0 depending on whether there is a tree present or not based on the current index and how large the right step is
        let findTree (lineNum : int) (terrain : slopeLand seq) : int = 
            let treePos = lineNum * rightStep
            let landToCheck = terrain |> Seq.item treePos
            match landToCheck.tree with
            | true -> 1
            | false -> 0
        downSteppedMap
        |> List.mapi findTree
        |> List.sum
        

    let parse (fileInput : string list) : slopeLand seq list =
        //Convert each line into an infinitely repeating sequence
        let lineToInfiniteSeq (inputString : string) : slopeLand seq = 
            let baseLine : slopeLand list = 
                let charList = inputString.ToCharArray() |> Array.toList
                charList
                |> List.map (fun c ->
                    match c with
                    | '#' -> {tree = true;}
                    | '.' -> {tree = false;}
                    | _ ->
                        raise (Unparsable ("couldn't parse lol"))
                        {tree = false;}
                    )
            Seq.initInfinite (fun i ->
                baseLine.Item(i % baseLine.Length)
                )
        fileInput
        |> List.map lineToInfiniteSeq

    

    let run : unit = 
        let fileName = "Advent2020D3.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        let inputSlopes = [{right = 1; down = 1;};{right = 3; down = 1;};{right = 5; down = 1;};{right = 7; down = 1;};{right = 1; down = 2;}]
        let trees = 
            inputSlopes
            |> List.map (treesHit initialState)
        let product = 
            let multTogether (lastProd : int) (currentEntry : int) = 
                lastProd * currentEntry
            (1, trees)
            ||> List.fold multTogether

        printfn "Sum: %i" (treesHit initialState {right = 3; down = 1;})
        printfn "Product: %i" product