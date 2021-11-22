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
        let initialRule = rules.TryFind ruleToCheck

    let run : unit = 
        let fileName = "Advent2020D19.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        
        printfn "%A" initialState