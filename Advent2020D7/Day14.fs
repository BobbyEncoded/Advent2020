namespace Advent2020.Day14

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions

module Main =

    let parse (fileInput : string list) =
        fileInput

    let run : unit = 
        let fileName = "Advent2020D14.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initalState = parse fileInput

        printfn "Test"