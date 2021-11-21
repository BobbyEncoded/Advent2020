namespace Advent2020.Day1

open System

type sumContainer = 
    {
        value1 : int;
        value2 : int;
        value3 : int;
        sum : int;
    }

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

    let productFromSums (inputInts : int list) : int =        
        let arrayInput = 
            inputInts
            |> List.toArray
        let valuesOfSum =
            arrayInput
            |> Array.allPairs arrayInput
            |> Array.allPairs arrayInput
            |> Array.map (fun n -> 
                match n with
                | (x, y) -> 
                    match y with
                    | (y, z) -> (x, y, z, x+y+z)
                )
            |> List.ofArray
            |> List.find (fun x -> 
                match x with
                | _, _, _, s -> s = 2020
                )
        match valuesOfSum with
        | a, b, c, _ -> a * b * c

    let parse (fileInput : string list) : int list =
        fileInput
        |> List.map Int32.Parse
    

    let run : unit = 
        let fileName = "Advent2020D1.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Product: %i" (productFromSums initialState)