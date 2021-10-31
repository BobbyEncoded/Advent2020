namespace Advent2020

open System

module Program =

    [<EntryPoint>]
    let main argv = 
        //Advent2020.Day7.Main.run
        //Advent2020.Day8.Main.run
        let timer = System.Diagnostics.Stopwatch.StartNew()
        Advent2020.Day4.Main.run
        timer.Stop()
        printfn "The program took %i ms to run." timer.ElapsedMilliseconds
        0