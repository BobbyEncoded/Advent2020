namespace Advent2020.Day15

open System
open Advent2020
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open HPCsharp

module Main =

    let parse (fileInput : string list) =
        fileInput

    let run : unit = 
        let fileName = "Advent2020D15.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Test"