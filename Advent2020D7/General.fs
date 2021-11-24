namespace Advent2020

open System

/// <summary>Holds a lesser and greater number, and methods to use with them.</summary>
/// <param name="lower">The lesser value.</param>
///<param name="upper">The greater value.</param>
[<StructAttribute>]
type Range<'T>(lower : 'T when 'T : comparison, upper : 'T) =
    member this.Lower = lower
    member this.Upper = upper

    /// <summary>
    /// Checks whether a value is in range of the parameters, inclusively.
    /// Values that are equal to a side of the range are considered in range.
    /// </summary>
    /// <param name="valToCheck">Value to check whether it is in range.</param>
    member this.InRangeInclusive (valToCheck : 'T) =
        (valToCheck >= this.Lower) && (valToCheck <= this.Upper)

    /// <summary>
    /// Checks whether a value is in range of the parameters, inclusively.
    /// Values that are equal to a side of the range are considered NOT in range.
    /// </summary>
    /// <param name="valToCheck">Value to check whether it is in range.</param>
    member this.InRangeExclusive (valToCheck : 'T) =
        (valToCheck > this.Lower) && (valToCheck < this.Upper)

module Range =
    let InRangeInclusive (baseRange : Range<'T>) (valToCheck : 'T) =
        baseRange.InRangeInclusive(valToCheck)
    let InRangeExclusive (baseRange : Range<'T>) (valToCheck : 'T) =
        baseRange.InRangeExclusive(valToCheck)

module Option =
    let (>>=) r f = Option.bind f r
    let rtn v     = Some v
     
    let traverseList f ls = 
        let folder head tail = f head >>= (fun h -> tail >>= (fun t -> h::t |> rtn))
        List.foldBack folder ls (rtn List.empty)
    let sequenceList ls = traverseList id ls
    // val traverseList : ('a -> 'b option) -> 'a list -> 'b list option
    // val sequenceList : 'a option list -> 'a list option

module List = 
    let product (list : int64 list) : int64 = 
        match list with
        | [] -> 0L
        | head::tail -> 
            let rec product (state : int64) (remainingList : int64 list) = 
                match remainingList with 
                | [] -> state
                | head::tail ->
                    let newState = state * head
                    product newState tail
            product head tail

module File =

    let userProfile = Environment.GetEnvironmentVariable("USERPROFILE")
    let downloadLocation = userProfile + @"\" + "Downloads"
    let getFullFilePath fileName = 
        downloadLocation + @"\" + fileName

    //Text Import
    let readLines (filePath:string) = seq {
        use sr = new System.IO.StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let listLines (filePath:string) = readLines filePath |> Seq.toList

    let listedLines fileName = getFullFilePath fileName |> listLines

module General = 

    exception Unresolvable of string

    let flip f a b = f b a

    let groupedStrings (rawInput : string list) : string list list = 
        let inputFolder (acc : string list list) (entryToAdd : string) : string list list =
            match entryToAdd with 
            | "" ->
                let emptyStringList = List<string>.Empty //Note that this is a property with capital E of List<string>
                let newEmptyStringList = List.singleton emptyStringList
                List.append acc newEmptyStringList
            | entryToAdd ->
                let lastStringListSplitIndex = 
                    ((acc |> List.length) - 1)
                let front, last = acc |> List.splitAt lastStringListSplitIndex
                let updateFinalString (stringListListToReplace : string list list) : string list list = 
                    stringListListToReplace
                    |> List.head
                    |> (flip List.append) (List.singleton entryToAdd)
                    |> List.singleton
                last
                |> updateFinalString
                |> List.append front
        let initialAcc : string list list =
            List.empty<string>  //While this is a function which takes a string type, it does the same thing.
            |> List.singleton
        (initialAcc, rawInput)
        ||> List.fold inputFolder