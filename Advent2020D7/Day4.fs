namespace Advent2020.Day2

open System

type passRule = 
    {
        min : int;
        max : int;
        character : char;
    }
    //for part 2, min is the first possible position, and max is the second possible position

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

    let checkValidPassword (inputRulePass : (passRule * string)) : bool = 
        let rule = fst inputRulePass
        let pass = snd inputRulePass
        let minAmt = rule.min
        let maxAmt = rule.max
        let charToCount = rule.character

        let passCharArray = pass.ToCharArray()
        let charInPos1 = 
            passCharArray
            |> Array.tryItem(minAmt-1)
        let charInPos2 = 
            passCharArray
            |> Array.tryItem(maxAmt-1)
        let char1Exists =
            match charInPos1 with 
            | Some pos1char -> pos1char.Equals(charToCount)
            | None -> false
        let char2Exists = 
            match charInPos2 with 
            | Some pos2char -> pos2char.Equals(charToCount)
            | None -> false
        if (char1Exists <> char2Exists) then true else false

    let countValidPasswords (inputRulesAndPasses : (passRule * string) list) : int = 
        inputRulesAndPasses
        |> List.map checkValidPassword
        |> List.map (fun b -> if b then 1 else 0)
        |> List.sum


    let parse (fileInput : string list) : (passRule * string) list =
        let rulePassSplit (inputString : string) = inputString.Split(':')
        let rulePassTuple (rulePassArray : string array) : (string * string) =
            (rulePassArray.[0], rulePassArray.[1])
        let ruleStringToRule (ruleString : string) : passRule =
            let elements = ruleString.Split([|'-';' '|])
            let minValue = elements.[0] |> Int32.Parse
            let maxValue = elements.[1] |> Int32.Parse
            let character = elements.[2]
            {min = minValue; max = maxValue; character = character.ToCharArray(0,1).[0]}
        let noSpacePass (spacePass : string) : string = 
            spacePass.Replace(" ", "")
        let turnTupleIntoRuleAndPass (inputTuple : string * string) : passRule * string =
            match inputTuple with 
            | inputRule, inputPass -> (ruleStringToRule inputRule, noSpacePass inputPass)
        fileInput
        |> List.map rulePassSplit
        |> List.map rulePassTuple
        |> List.map turnTupleIntoRuleAndPass
    

    let run : unit = 
        let fileName = "Advent2020D2.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Sum: %i" (countValidPasswords initialState)