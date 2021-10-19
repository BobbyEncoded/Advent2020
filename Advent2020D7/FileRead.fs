namespace Advent2020

open System

module File =

    let userProfile = Environment.GetEnvironmentVariable("USERPROFILE")
    let downloadLocation = userProfile + @"\" + "Downloads"
    let getFullFilePath fileName = 
        downloadLocation + @"\" + fileName

    //Text Import
    let readLines (filePath:string) = seq {
        use sr = new System.IO.StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let listLines (filePath:string) = readLines filePath |> Seq.toList

    let listedLines fileName = getFullFilePath fileName |> listLines