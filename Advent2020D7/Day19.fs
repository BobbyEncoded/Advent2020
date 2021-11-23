namespace Advent2020.Day19

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions

type Rule = 
    | Letter of char
    | RuleCol of RuleCol
and RuleCol =
    {
        FirstRules : int list
        OrRules : int list option
    }

module Main =

    exception Unresolvable

    let parse (fileInput : string list) =
        let separatedStrings = General.groupedStrings(fileInput)
        let ruleStrings = separatedStrings.Item 0
        let messages = separatedStrings.Item 1

        let getRuleFromRuleString (inputString : string) = 
            let splitString = inputString.Split(':')
            let index = splitString.[0] |> Int32.Parse
            let rules = splitString.[1]
            let splitBySpace = rules.Split(' ') |> Array.filter (fun x -> x |> String.IsNullOrWhiteSpace |> not)
            let onlyElement = splitBySpace |> Array.tryExactlyOne
            match onlyElement with
            | Some x when x.StartsWith("\"") -> index, Rule.Letter(x.Chars(1))
            | Some x -> index, Rule.RuleCol{FirstRules = Int32.Parse x |> List.singleton; OrRules = None}
            | None ->
                let findPipeIndex = splitBySpace |> Array.tryFindIndex (fun x -> x.Contains(@"|"))
                let parseAndList inputArray = inputArray |> Array.map Int32.Parse |> List.ofArray
                match findPipeIndex with 
                | None -> index, Rule.RuleCol{FirstRules = splitBySpace |> parseAndList; OrRules = None}
                | Some pipeIndex ->
                    let firstRules, orRules = splitBySpace |> Array.splitAt pipeIndex
                    index, Rule.RuleCol{FirstRules = firstRules |> parseAndList; OrRules = Some(orRules |> Array.tail |> parseAndList)}

        let rules = ruleStrings |> List.map getRuleFromRuleString |> Map.ofList
        //let messageChars = messages |> List.map (fun x -> x.ToCharArray() |> List.ofArray)
        rules, messages

    let maxEntrySize (messages : string list) = messages |> List.maxBy (fun x -> x.Length) |> String.length
                
    let findPossibleCombinations (rules : Map<int, Rule>) (ruleToCheck : int) (maxEntrySize : int) = 
        let initialRule = rules |> Map.find ruleToCheck
        let initialSet = Set.empty<string>
        let initialPrevRuleSet = Set.empty<Rule list>

        let rec createCombos2 (baseRule : Rule list Set) (setOfCompletedBranches : string Set) (prevRuleSet : Rule list Set) = 
            let trimmedTooLongRules =
                baseRule
                |> Set.filter (fun x -> List.length x <= maxEntrySize)
            let currentRuleList = trimmedTooLongRules |> List.ofSeq
            // Check if we have resolved infinite loops
            let fullyDissolved = 
                Set.isSuperset prevRuleSet trimmedTooLongRules
            match fullyDissolved with
            | true -> setOfCompletedBranches
            | false ->
                let explodeARuleList (flattenedRuleList : Rule list) = //flattenedRuleList needs to be a list of flattened rules, and we will now unflatten them into their components
                    let flattenedRuleSeq = [
                            for rule in flattenedRuleList do
                                match rule with
                                | Rule.Letter x -> yield (Rule.Letter x) //If it's a letter then map the element to a letter
                                | Rule.RuleCol x -> //If it's a Rule Collection
                                    match x.OrRules with
                                    | Some _ -> raise Unresolvable //If there's extra rules, throw an exception
                                    | None ->
                                        let rulesToSearch = x.FirstRules // If there's only primary rules in this rule
                                        let newRuleList = // Look up each rule in the higher level rule and make a list of those found rules
                                            rulesToSearch
                                            |> List.map (fun index ->
                                                rules
                                                |> Map.find index
                                                )
                                        yield! newRuleList //We can yield a collection of rules through the computation expression
                        ]
                    flattenedRuleSeq
                let flattenRules (unflattenedRuleList : Rule list) = //This is the unflattened rule list from the previous function, which we will now do math on to pass along in a flat form.
                    let acc = List<Rule>.Empty |> List.singleton
                    (acc, unflattenedRuleList)
                    ||> List.fold (fun acc ruleToAdd ->
                        match ruleToAdd with
                        | Letter _ -> 
                            acc
                            |> List.map (fun x ->
                                ruleToAdd
                                |> List.singleton
                                |> List.append x
                                )
                        | RuleCol rc ->
                            let accAddFunc (ruleList : int list) (accToAdd : Rule list list) = 
                                accToAdd
                                |> List.map (fun x ->
                                    Rule.RuleCol{FirstRules = ruleList; OrRules = None}
                                    |> List.singleton
                                    |> List.append x
                                    )
                            match rc.OrRules with
                            | None ->
                                acc
                                |> accAddFunc rc.FirstRules
                            | Some orRules ->
                                let firstSet = 
                                    acc
                                    |> accAddFunc rc.FirstRules
                                let orSet = 
                                    acc
                                    |> accAddFunc orRules
                                List.append firstSet orSet
                        )
                let updatedRules = 
                    currentRuleList
                    |> List.map explodeARuleList
                    |> List.map flattenRules
                    |> List.concat
                let checkForCompletedRuleSets (flattenedRules : Rule list list) = 
                    let splitByBeingComplete (listToCheck : Rule list) = 
                        listToCheck
                        |> List.forall (fun x ->
                            match x with
                            | Letter _ -> true
                            | RuleCol _ -> false
                            )
                    let listsToAdd, listsToPass = 
                        flattenedRules
                        |> List.partition splitByBeingComplete
                    let setToReturn = 
                        listsToAdd
                        |> Set.ofList
                        |> Set.map (fun x ->
                            x
                            |> List.map (fun y ->
                                    match y with
                                    | Letter c -> c
                                    | RuleCol _ -> raise Unresolvable
                                )
                            |> Array.ofList
                            |> System.String
                            )
                        |> Set.union setOfCompletedBranches
                    setToReturn, listsToPass
                let newSet, nextRules = 
                    updatedRules
                    |> checkForCompletedRuleSets
                createCombos2 (nextRules |> Set.ofList) newSet trimmedTooLongRules

        createCombos2 (initialRule |> List.singleton |> List.singleton |> Set.ofList) initialSet initialPrevRuleSet

    let sumEntriesAgainstSet (validCombinations : string Set) (inputEntries : string list) = 
        let hashSetCombos = new System.Collections.Generic.HashSet<string>(validCombinations)
        printfn "Num Combos: %i" (hashSetCombos.Count)
        inputEntries
        |> List.map hashSetCombos.Contains
        |> List.map (fun x -> if x then 1 else 0)
        |> List.sum

    let updateMapForPart2 (initialMap : Map<int, Rule>) = 
        initialMap
        |> Map.change 8 (fun _ ->
            Rule.RuleCol{FirstRules = [42]; OrRules = Some[42; 8]} |> Some)
        |> Map.change 11 (fun _ ->
            Rule.RuleCol{FirstRules = [42; 31]; OrRules = Some[42; 11; 31]} |> Some)

    let run : unit = 
        let fileName = "Advent2020D19Test2.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialMap, initialEntries = parse fileInput
        let maxSize = initialEntries |> maxEntrySize
        let part2Map = initialMap |> updateMapForPart2
        let combinations = findPossibleCombinations initialMap 0 maxSize
        
        printfn "%i" (sumEntriesAgainstSet combinations initialEntries)