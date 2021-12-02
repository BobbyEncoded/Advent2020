namespace Advent2021.Day2

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

type SubInstruction =
    | Forward of int
    | Down of int
    | Up of int

type Position = 
    {
        horPos : int
        depth : int
        aim : int
    }

module Main =

    let parse (fileInput : string list) =
        fileInput
        |> List.map (fun s ->
            let instructionString = Regex.Match(s,"^\w+").Value
            let instructionValue = Regex.Match(s, "\d+$").Value |> Int32.Parse
            match instructionString with
            | "forward" -> SubInstruction.Forward (instructionValue)
            | "down" -> SubInstruction.Down (instructionValue)
            | "up" -> SubInstruction.Up (instructionValue)
            | _ -> raise (General.Unresolvable("bad instruction"))
            )

    let part1 (initInsts : SubInstruction list) = 
        let initPos = {horPos = 0; depth = 0; aim = 0}
        let instructionRunner (inputPos : Position) (currentInstruction : SubInstruction) =
            match currentInstruction with
            | Forward i -> {inputPos with horPos = inputPos.horPos + i; depth = inputPos.depth + (i * inputPos.aim)}
            | Down i -> {inputPos with aim = inputPos.aim + i}
            | Up i -> {inputPos with aim = inputPos.aim - i}
        (initPos, initInsts)
        ||> List.fold instructionRunner

    let multDepthPos (inputPos : Position) : int64 = 
        let pos = inputPos.horPos |> int64
        let depth = inputPos.depth |> int64
        pos * depth

    let run : unit = 
        let fileName = "Advent2021D2.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        let part1ans = initialState |> part1 |> multDepthPos

        printfn "%A" part1ans