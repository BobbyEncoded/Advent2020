namespace Advent2020.Day5

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

    exception Unparsable of string

    let getSeatIDFromSeat (seat : seat) : int = 
        (8 * seat.row) + seat.col

    let findLargestId (seatList : seat list) : int = 
        seatList
        |> List.map getSeatIDFromSeat
        |> List.max

    let parse (fileInput : string list) : seat list =
        let splitSeatString (inputSeatString : string) : string * string =
            inputSeatString.Substring(0,7), inputSeatString.Substring(7)
        let folderForPos (inputList : 'a list) (charToNavigate : char) : 'a list = 
            let splitList = inputList |> List.splitInto 2
            match charToNavigate with
            |'F' |'L' -> splitList |> List.head
            |'B' |'R' -> splitList |> List.last
            | _ -> raise (Unparsable ("couldn't parse lol"))

        let initialSeatRows = [ 0 .. 127 ]
        let initialSeatCol = [ 0 .. 7 ]
        
        let getSeatCoordFromString (cleaveArray : int list) (coordString : string) : int =
            let charCommands = coordString.ToCharArray()
            (cleaveArray, charCommands)
            ||> Array.fold folderForPos
            |> List.exactlyOne

        let (rowStringList, colStringList) =
            fileInput
            |> List.map splitSeatString
            |> List.unzip

        let rowNums = 
            rowStringList
            |> List.map (getSeatCoordFromString initialSeatRows)

        let colNums = 
            colStringList
            |> List.map (getSeatCoordFromString initialSeatCol)

        (rowNums, colNums)
        ||> List.map2 (fun row col -> {row = row; col = col})

    

    let run : unit = 
        let fileName = "Advent2020D5.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Largest ID: %i" (findLargestId initialState)