namespace Advent2020

open System

module Program =

    [<EntryPoint>]
    let main argv = 
        let timer = System.Diagnostics.Stopwatch.StartNew()
        Advent2021.Day3.Main.run
        timer.Stop()
        printfn "The program took %i ms to run." timer.ElapsedMilliseconds
        0