

namespace Advent2020.Day18

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open FParsec

type calcChar =
    | LeftParenth
    | RightParenth
    | Mult
    | Add
    | Val of int

type expressionEntry = 
    | Character of calcChar
    | Expression of expressionEntry list

type Operator = Add | Mul
type OperationData = {operator : Operator; left : Expression; right : Expression}
and Expression = 
    | Value of int64
    | Operation of OperationData
    | Parentheses of Expression

module Main =

    let fromInternet (input : string array) = 
        
        // parsers
        let parseUInt64 : Parser<uint64, unit> = puint64 |>> uint64
        let removeWhitespace s = pstring s >>. spaces
        // Function that will take in the string s and parse for it.  Then, we will run the parsed result into a parser which removes whitespace, and return the new result.
        // This function will handle the removal of whitespace around characters we parse, and return the string after removal.

        // We will make a bigger parsing function which consumes an expression.
        // This will take our integer parser, and remove any whitespace before it.
        // If there is no int to parse, then we will perform a different parse check.
        // We will parse for an opening parenthesis, then a closing parenthesis, and finally an expression parse, and return the parsed result from after the closing parenthesis.
        let parseTerm expression = (parseUInt64 .>> spaces) <|> between (removeWhitespace "(") (removeWhitespace ")") expression
        
        //Create a function which will run the parser and return the appropriate type
        let runParser expr str =
            //run is a method from FParsec, and quickly runs the parser with basic parameters on the string provided.
            match run expr str with
            | Success (result, _ , _) -> result
            | Failure (errorMsg, _, _) -> failwithf "Error from parser: %s" errorMsg
        

        let runPart input =
            let opp = OperatorPrecedenceParser<uint64, unit, unit>()
            let expression = opp.ExpressionParser //The expression parser.  
        
            let addOperator = InfixOperator ("+", spaces, 2, Associativity.Left, (+)) //Precedence 2 is for part 2, where addition is evaluated first
            //Infix Operator is FParsec's binary infix operator definition.  We can use this to create the Parsers.
            //Whenever we see a + sign, we shall ignore spaces near it.  It shall be run at level 1 precedence (basically order of operations).
            //Solve from left to right.
            //When you see a "+", the operation to perform is the + operation.
            let multOperator = InfixOperator ("*", spaces, 1, Associativity.Left, (*))
            // Multiplication with level 1 precedence will be evaluated at the same time as addition.
        
            opp.TermParser <- parseTerm expression
            // This parser is called to parse the terms in between the operators.
            // There is no default, so you must set this parser before you can call the ExpressionParser.
            // Note that the term parser is also expected to parse any whitespace after a term. 
            opp.AddOperator(addOperator)
            opp.AddOperator(multOperator)
        
            input
            |> Array.sumBy (runParser expression) 
            |> printfn "%A"
        
        runPart input
        

    //let solveExpressionEntry (inputEntries : expressionEntry list) : int = 
        

    let parse (fileInput : string list) : calcChar list list =
        let stringToCalcCharList (inputString : string) = 
            let noWhitespace = inputString.Replace(" ", String.Empty)
            let charOrder = noWhitespace.ToCharArray() |> List.ofArray
            charOrder
            |> List.map (fun x ->
                match x with
                | '(' -> LeftParenth
                | ')' -> RightParenth
                | '*' -> Mult
                | '+' -> calcChar.Add
                | x -> Val(Int32.Parse(x.ToString()))
                )
        fileInput
        |> List.map stringToCalcCharList

    let run : unit = 
        let fileName = "Advent2020D18.txt"
        let fileInput = Advent2020.File.listedLines fileName

        fileInput
        |> Array.ofList
        |> fromInternet

        // printfn "Value: %i" value