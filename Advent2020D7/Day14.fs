namespace Advent2020.Day14

open System
open Advent2020 //Contains Range type and accessors
open System.Text.RegularExpressions //Allows usage of Regular Expressions
open System.Linq

type MaskMem =
    | Mask of string
    | Mem of address : uint64 * value : uint64

    static member convertUInt64ToBoolArray (inputUInt64 : uint64) = 
        let mutable boolArray = Array.init 36 (fun z -> false)
        let mutable bitArray = 
            inputUInt64
            |> BitConverter.GetBytes
            |> System.Collections.BitArray
        bitArray.Length <- 36
        bitArray.CopyTo(boolArray, 0)
        boolArray

    member this.MaskArray = 
        match this with
        | Mem (_ , _) -> raise (General.Unresolvable("Tried to get Mask of a Mem type"))
        | Mask x -> x.ToCharArray() |> Array.rev

    member this.ValArray = 
        match this with
        | Mem (_ , x) -> MaskMem.convertUInt64ToBoolArray(x)
        | Mask _ -> raise (General.Unresolvable("Tried to get Val of a Mask type"))

    member this.AddressArray = 
        match this with
        | Mem (x , _) -> MaskMem.convertUInt64ToBoolArray(x)
        | Mask _ -> raise (General.Unresolvable("Tried to get Val of a Mask type"))

    static member private MaskAValue (inputMask : char array) (inputValue : bool array) = 
        let performOperationOnBit (inputChar : char) (inputBit : bool) = 
            match inputChar with
            | 'x' | 'X' -> inputBit
            | '0' -> false
            | '1' -> true
            | _ -> raise (General.Unresolvable("Mask contains characters which aren't 'x', 'X', '0', or '1'"))

        (inputMask, inputValue)
        ||> Array.map2 performOperationOnBit

    member this.MaskMemValue (otherEntry : MaskMem) = 
        match (this, otherEntry) with
        | (MaskMem.Mask _, MaskMem.Mem (_, _)) -> MaskMem.MaskAValue this.MaskArray  otherEntry.ValArray
        | (MaskMem.Mem (_, _), MaskMem.Mask _) -> MaskMem.MaskAValue otherEntry.MaskArray  this.ValArray
        | _ -> raise (General.Unresolvable("Cannot Mask and Mem on two MaskMems of the same type"))

    member this.MaskMemValue (maskString : string) = 
        match this with
        | (MaskMem.Mask _) -> raise (General.Unresolvable("Cannot Mask and Mem on two MaskMems of the same type"))
        | (MaskMem.Mem (_, thisValue)) -> MaskMem.MaskAValue (maskString.ToCharArray() |> Array.rev) (MaskMem.convertUInt64ToBoolArray(thisValue))

    static member MaskMemValue (thisEntry : MaskMem, otherEntry : MaskMem) = 
        thisEntry.MaskMemValue otherEntry

    static member convertBoolsToUInt64 (inputBoolArray : bool array) : uint64 = 
        let mutable convertedUINT : uint64 = 0UL
        for i = 0 to inputBoolArray.Length - 1 do
            convertedUINT <- convertedUINT + ((pown 2UL i) * (if inputBoolArray[i] then 1UL else 0UL))
        convertedUINT

        

module Main =

    let parse (fileInput : string list) =
        let turnStringIntoMaskMem (inputString : string) = 
            let noSpaceString = inputString.Replace(" ", "")
            let splitString = noSpaceString.Split('=')
            let location = splitString.[0]
            let valToUse = splitString.[1]
            match location.StartsWith("mask") with
            | true -> MaskMem.Mask valToUse
            | false ->
                let parsedMem = Regex.Match(location, @"\d+").Value
                MaskMem.Mem(UInt64.Parse(parsedMem), UInt64.Parse(valToUse))

        fileInput
        |> List.map turnStringIntoMaskMem

    let runInstructionsInMemory (inputInstructions : MaskMem list) = 
        let memory = Map.empty<uint64, uint64>
        let mostRecentMask = String.Empty

        let instructionFolder (memAndMaskTuple : Map<uint64, uint64> * string) (instructionToExecute : MaskMem) = 
            let (mem, lastMask) = memAndMaskTuple
            match instructionToExecute with
            | Mask newMask -> (mem, newMask)
            | Mem (address, _) ->
                let newVal = MaskMem.convertBoolsToUInt64(instructionToExecute.MaskMemValue(lastMask))
                (mem.Add(address, newVal), lastMask)

        let updatedMemory, _ = 
            ((memory, mostRecentMask), inputInstructions)
            ||> List.fold instructionFolder

        let getSumOfMemory = 
            let addresses, values = 
                updatedMemory
                |> Map.toList
                |> List.unzip
            values
            |> List.sum

        getSumOfMemory

    let run : unit = 
        let fileName = "Advent2020D14.txt"
        let fileInput = Advent2020.File.listedLines fileName
        let initialState = parse fileInput

        printfn "Sum: %i" (runInstructionsInMemory(initialState))