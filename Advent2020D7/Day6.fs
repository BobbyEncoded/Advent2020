namespace Advent2020.Day6

open System

type seat = 
    {
        row : int;
        col : int;
    }



(*
module private Day4Helpers = 
    type fieldAndVal = 
        {
            field : string;
            stringVal : string;
        }
*)


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


    let parse (fileInput : string list) : seat list =
        

    

    let run : unit = 
        let fileName = "Advent2020D5.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        //alternateMissingID initialState
        printfn "Missing ID: %i" 