namespace Advent2020.Day18

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions

module Main =

    let findTicketScanningErrorRate (rules : Map<string,(Range<int> * Range<int>)>) (nearbyTickets : int list list) : int =
        let allValidRanges : Range<int> list =
            let (range1, range2) = 
                rules
                |> Map.toList
                |> List.map snd
                |> List.unzip
            range2
            |> List.append range1

        let allNearbyTicketFields = 
            nearbyTickets
            |> List.concat

        let checkIfFieldIsValid (rangesList : Range<int> list) (field : int) : bool = 
            rangesList
            |> List.map (fun x -> x.InRangeInclusive field)
            |> List.contains true
            
        let valueForValidFields (field : int) = 
            let validity = checkIfFieldIsValid allValidRanges field
            if validity then 0 else field

        allNearbyTicketFields
        |> List.map valueForValidFields
        |> List.sum

    let findValidTickets (rules : Map<string,(Range<int> * Range<int>)>) (nearbyTickets : int list list) =
        let allValidRanges : Range<int> list =
            let (range1, range2) = 
                rules
                |> Map.toList
                |> List.map snd
                |> List.unzip
            range2
            |> List.append range1

        let checkIfFieldIsValid (rangesList : Range<int> list) (field : int) : bool = 
            rangesList
            |> List.map (fun x -> x.InRangeInclusive field)
            |> List.contains true

        nearbyTickets
        |> List.filter (fun x ->
            x
            |> List.map (checkIfFieldIsValid allValidRanges)
            |> List.forall id
            )

    let matchFieldsToRules (rules : Map<string,(Range<int> * Range<int>)>) (validTickets : int list list) = // : string Set list = 
        let ruleEntries = validTickets |> List.transpose //This is a grouping of all entries in an individual rule
        //We will need to find which entry correlates to each grouping of rules
        let findRulesTheEntriesMatch (entries : int list) : string Set =
            let entryValidRules (entry : int) = 
                rules
                |> Map.filter(fun _ ranges ->
                    let range1, range2 = ranges
                    range1.InRangeInclusive(entry) || range2.InRangeInclusive(entry)
                    )
                |> Map.keys
                |> Set.ofSeq
            entries
            |> List.map entryValidRules
            |> Set.intersectMany

        let validRuleConfigs = 
            ruleEntries
            |> List.map findRulesTheEntriesMatch

        let getAllSingletonEntries (validRuleConfigs : string Set list) : string list =
            let ruleMatchInit = validRuleConfigs |> List.map (fun _ -> "") //Empty list of strings to hold the field name
            let rec removeSingletonElements (currentValidRules : string list) (currentPossibleRules : string Set list) = 
                let filledList = currentValidRules |> List.contains ""
                match filledList with 
                | false -> (currentValidRules, currentPossibleRules)
                | true ->
                    let singletonRuleIndex = 
                        currentPossibleRules
                        |> List.findIndex (fun x -> x.Count = 1)
                    let element =
                        currentPossibleRules
                        |> List.item singletonRuleIndex
                        |> Set.toList
                        |> List.exactlyOne
                    let newValidRules =
                        currentValidRules
                        |> List.updateAt singletonRuleIndex element
                    let newCurrentPossibleRules = 
                        currentPossibleRules
                        |> List.map (Set.remove element)
                    removeSingletonElements newValidRules newCurrentPossibleRules
            let (rulesList, _) = removeSingletonElements ruleMatchInit validRuleConfigs
            rulesList

        //printfn "%A" (getAllSingletonEntries validRuleConfigs)

        getAllSingletonEntries validRuleConfigs |> List.indexed

    let departureProduct (myTicket : int list) (departureIndices : (int * string) list) : int64 = 
        let indicesToCheck = 
            departureIndices
            |> List.filter (fun (_, fieldName) ->
                fieldName.StartsWith("departure")
                )
            |> List.map fst
        indicesToCheck
        |> List.map (fun x -> myTicket |> List.item x)
        |> List.map int64
        |> List.product

    let parse (fileInput : string list) =
        let splitOnComma (inputString : string) =
            let getNumsRegex = new Regex("\d{1,}")
            inputString
            |> getNumsRegex.Matches
            :> System.Collections.Generic.IList<Match>
            |> List.ofSeq
            |> List.map (fun x -> x.Value)
            |> List.map Int32.Parse

        let getTicketRuleFromString (inputString : string) =
            let getAllToColonRegex = new Regex(".+?(?=:)")
            let fieldName = inputString |> getAllToColonRegex.Match |> (fun x -> x.Value)
            let getMatchAsInt (item : int) = 
                let getNumsRegex = new Regex("\d{1,}")
                let matches = inputString |> getNumsRegex.Matches
                matches.Item(item).Value |> Int32.Parse
            let firstRange = Range<int>(getMatchAsInt 0, getMatchAsInt 1)
            let secondRange = Range<int>(getMatchAsInt 2, getMatchAsInt 3)
            (fieldName, (firstRange, secondRange))

        let inputStringList = fileInput |> Advent2020.General.groupedStrings

        let ruleStrings = inputStringList.Item 0
        let myTicketString = inputStringList.Item 1
        let nearbyTicketStrings = inputStringList.Item 2

        let myTicket = 
            myTicketString
            |> List.tail
            |> List.exactlyOne
            |> splitOnComma

        let nearbyTickets = 
            nearbyTicketStrings
            |> List.tail
            |> List.map splitOnComma

        let rules = ruleStrings |> List.map getTicketRuleFromString |> Map.ofList

        //printfn "My Ticket: %A" myTicket
        //printfn "Nearby Ticket: %A" nearbyTickets
        //printfn "Rules: %A" rules

        (rules, myTicket, nearbyTickets)

    let run : unit = 
        let fileName = "Advent2020D16.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let (rules, myTicket, nearbyTickets) = parse fileInput
        
        let departureIndices = ((rules, nearbyTickets) ||> findValidTickets) |> (matchFieldsToRules rules)
        let product = departureProduct myTicket departureIndices

        printfn "Product: %i" product
        // printfn "Ticket Scanning Error Rate: %A" ((rules, nearbyTickets) ||> findValidTickets)

        //printfn "Sum: %i" (countQuestionsAnswerByAll initialState)