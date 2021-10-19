// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
namespace Advent2020.Day7

open System

type childBag = 
    {
        Count: int;
        BagColor: string;
    }

type Rule = 
     {
         ParentBag: string;
         ChildrenBags: childBag list option;
     }

module Option =
    let (>>=) r f = Option.bind f r
    let rtn v     = Some v
     
    let traverseList f ls = 
        let folder head tail = f head >>= (fun h -> tail >>= (fun t -> h::t |> rtn))
        List.foldBack folder ls (rtn List.empty)
    let sequenceList ls = traverseList id ls
    // val traverseList : ('a -> 'b option) -> 'a list -> 'b list option
    // val sequenceList : 'a option list -> 'a list option

module Main =
    let run =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let userProfile = Environment.GetEnvironmentVariable("USERPROFILE")
        let downloadLocation = userProfile + @"\" + "Downloads"
        let fileName = "Advent2020D7.txt"
        let fullFilePath = downloadLocation + @"\" + fileName

        //Text Import
        let readLines (filePath:string) = seq {
            use sr = new System.IO.StreamReader (filePath)
            while not sr.EndOfStream do
                yield sr.ReadLine ()
        }

        let listLines (filePath:string) = readLines filePath |> Seq.toList

        let listedLines = listLines fullFilePath


        //Start of Text Splitting Rules--------------------------------------------------------------------
        let splitByContain (initString : string) : string[] = 
            initString.Split(" contain ", 2, StringSplitOptions.RemoveEmptyEntries)

        let trimBag (bagString : string) = 
            bagString.Replace(" bags","").Replace(" bag","").Replace(".","")

        //childBagString will need to be the string for all the child bags in a line
        let childBagStringToChildrenBags (childBagString : string) : childBag list option =
            let childBagStrings = childBagString.Split(", ")
            let childBagStringList = Array.toList childBagStrings
            let childBagStringToChildBag (childBagString : string) : childBag option =
                let childBagStringSplit = childBagString.Split(' ')
                let count = System.Int32.TryParse childBagStringSplit.[0]
                match count with 
                | false, _ -> None
                | true, countVal -> 
                    let colorStrings = childBagString.[1..]
                    let colorString = String.Concat(" ", colorStrings)
                    let colorStringTrimmed = colorString.Trim(' ')
                    Some { Count = countVal; BagColor = colorStringTrimmed}
            let childStringToChildBag = List.map childBagStringToChildBag
            Option.sequenceList (childStringToChildBag childBagStringList)


        let ruleStringToRule (ruleString : string) : Rule = 
            let trimmedString = trimBag ruleString
            let splitString = splitByContain trimmedString
            let childBagsLocal = childBagStringToChildrenBags splitString.[1]
            { ParentBag = splitString.[0]; ChildrenBags = childBagsLocal }
        //End of Text Splitting Rules ------------------------------------------------------------
 

        let allBagRules (ruleStrings : string list) : Rule list = 
            let stringToRuleMap = List.map ruleStringToRule
            stringToRuleMap ruleStrings

        let givenRules = allBagRules listedLines
        //Given rules is the text data we're given in Rule format

        //Given rules is the master data structure holding the GOODS
        let parentSet (listOfRules : Rule list) (stringToFind : string) : string Set =
            let ruleContainsStringToFind (stringFindInRule : string) (rule : Rule) : bool = 
                match rule.ChildrenBags with
                | None -> false
                | Some childBagList ->
                    let childBagIsColor (childBag : childBag) : bool =
                        childBag.BagColor.Equals stringFindInRule
                    List.exists childBagIsColor childBagList
            let ruleFound = ruleContainsStringToFind stringToFind
            let filteredRuleList = List.filter ruleFound listOfRules
            let parentOfRule (rule : Rule) : string =
                rule.ParentBag
            let parentList = List.map parentOfRule filteredRuleList
            (Set.empty, parentList) ||> List.fold (fun x y -> x.Add(y)) //This function adds each element of the list to a set

        let listOfParents = parentSet givenRules "shiny gold"

        let printFunc toPrint =
            printfn "%s" toPrint

        let findAllPotentialParents (ruleList : Rule list) (child : string) : Set<string> =
            let initSet = Set.empty.Add(child)
            let rec parentFindLoop accSet = //This loop will find a set of parents from a set of all possible children
                let parentFindSet = parentSet ruleList
                let doneSetOfSets = Seq.map parentFindSet accSet
                let recParentSet = Set.unionMany doneSetOfSets
                let recParentAndChildSet = Set.union recParentSet accSet
                match recParentAndChildSet.IsSubsetOf accSet with 
                | true -> recParentAndChildSet
                | false -> parentFindLoop recParentAndChildSet
            let allCandidatesAndChild = parentFindLoop initSet
            allCandidatesAndChild.Remove(child)

    
        let listOfAllParents = findAllPotentialParents givenRules "shiny gold"

        let countParentsAnswer = 
            Set.map printFunc listOfAllParents |> ignore
            printfn "Count: %s" (listOfAllParents.Count.ToString())

        //Part 2 code
        let countAllPotentialChildren (ruleList : Rule list) (child : string) : int =
            //Create a map for easy lookups
            let mapParents : Map<string, Rule> =
                ruleList
                |> List.map (fun a -> a.ParentBag, a)
                |> Map.ofList
            //I need a function which will count how many bags its children contains, and then I will need to recur it on each child element
            //This function will take in the name of a parent, and count the children.  It will recur and count the children's children if the children exist.  It will reply with 1 if it has no children.
            let rec countChildren (currentParent : string) : int =
                match mapParents.TryFind(currentParent) with
                | None -> 1
                | Some rule -> 
                    match rule.ChildrenBags with
                    | None -> 1
                    | Some childrenBags -> 
                        let sumOfBags = 
                            childrenBags
                            |> List.map (fun childBag -> (childBag.Count * (countChildren childBag.BagColor)))
                            |> List.sum
                        printfn "Current List Sum: %i" sumOfBags
                        1 + sumOfBags //Add 1 for the bag itself that it's in

            (countChildren child) - 1 //Subtract 1 for initial bag

        let countAllPotChildrenAns = 
            printfn "Child of parent count: %i" (countAllPotentialChildren givenRules "shiny gold")

        //countParentsAnswer
        countAllPotChildrenAns
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds