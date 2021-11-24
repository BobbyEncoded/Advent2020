namespace Advent2020.Day20

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq        

module Main =

    let parse (fileInput : string list) =
        fileInput

    let run : unit = 
        let fileName = "Advent2020D20.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Test"