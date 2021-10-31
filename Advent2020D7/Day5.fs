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

    let getListOfSeatIDs (seatList : seat list) : int list = 
        seatList
        |> List.map getSeatIDFromSeat

    let missingID (seatList : seat list) : int = 
        let listOfIDs = getListOfSeatIDs seatList |> List.sort
        let windowedIDs = listOfIDs |> List.windowed 3
        let verifyWindow (windowToVerify : int list) : bool = 
            let lowerVal = List.item 0 windowToVerify
            let midVal = List.item 1 windowToVerify
            let upperVal = List.item 2 windowToVerify
            let rangeDifference = upperVal - lowerVal
            let lowerDifference = midVal - lowerVal
            let upperDifference = upperVal - midVal

            //if (lowerVal = 726) then System.Diagnostics.Debugger.Break() else ()
            let check = (lowerDifference = 2)
            //printfn "LowVal: %i" lowerVal
            //printfn "MidVal: %i" midVal
            //printfn "UpVal: %i" upperVal
            //printfn "%b" check
            if check then true else false

        windowedIDs
        |> List.find verifyWindow
        |> List.item 0
        |> (+) 1

    let alternateMissingID (seatList : seat list) : unit = 
        let listOfIDs = getListOfSeatIDs seatList |> List.sort
        listOfIDs
        |> List.iteri (fun i seat -> if (((List.item (i+1) listOfIDs) - seat) <> 1) then printfn "%i" seat else () )


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

        //alternateMissingID initialState
        printfn "Missing ID: %i" (missingID initialState)