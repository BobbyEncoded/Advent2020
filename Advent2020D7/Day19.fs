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
        let messageChars = messages |> List.map (fun x -> x.ToCharArray() |> List.ofArray)
        rules, messageChars
                
    let findPossibleCombinations (rules : Map<int, Rule>) (ruleToCheck : int) = 
        let initialRule = rules |> Map.find ruleToCheck
        let flippedFind = rules |> (General.flip Map.find)

        let rec createCombinations (baseRule : Rule) = //(possibleVariations : char list list) = 
            let getCombosFromRules rulesFound =
                rulesFound
                |> List.map (fun x -> createCombinations x)
                |> List.concat
            match baseRule with 
            | Letter c -> [[c]]
            | RuleCol rules -> 
                match rules.OrRules with
                | None ->
                    let rulesFound = rules.FirstRules |> List.map flippedFind
                    rulesFound |> getCombosFromRules
                | Some orRules ->
                    let firstRulesFound = rules.FirstRules |> List.map flippedFind
                    let orRulesFound = orRules |> List.map flippedFind
                    let firstRuleCombos = firstRulesFound |> getCombosFromRules
                    let orRuleCombos = orRulesFound |> getCombosFromRules
                    List.append firstRuleCombos orRuleCombos

        let rec createCombos2 (baseRule : Rule list list) = 
            // First check if we have resolved everything into characters
            let fullyDissolved = 
                baseRule
                |> List.forall (fun x ->
                    x
                    |> List.forall (fun y ->
                        match y with
                        | Rule.Letter -> true
                        | Rule.RuleCol -> false
                        )
                    )
            match fullyDissolved with
            | true -> //If we fully dissolved everything, we can return the list of correct sequences.  We now have a list of a list of Rules, where all the Rules are characters.
                baseRule
                |> List.map (fun x -> 
                    let acc = List<char>.Empty
                    (acc, x)
                    ||> List.fold (fun acc x ->
                        match x with
                        | Letter 
                        )
                    )

                    

        createCombos2 (initialRule |> List.singleton)

    let run : unit = 
        let fileName = "Advent2020D19Test.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        
        printfn "%A" (findPossibleCombinations (fst initialState) 0)