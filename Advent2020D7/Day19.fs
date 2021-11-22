﻿namespace Advent2020.Day19

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions

module Main =

    let parse (fileInput : string list) =
        fileInput

    let run : unit = 
        let fileName = "Advent2020D19.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput
        
        printfn "%A" initialState