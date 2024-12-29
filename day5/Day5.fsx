type PageNumber = int
type OrderingRule = {Before: PageNumber; After: PageNumber}
type Rules = OrderingRule list
type Update = PageNumber list
type Updates = Update list
let file = System.IO.File.ReadAllText("day5/testinput.txt").Split("\n\n")
let parseOrderingRule (pair: string) : OrderingRule =
    let parts = pair.Split("|")
    let before = parts[0] |> int
    let after = parts[1] |> int
    { Before = before; After = after }

let parseUpdate (line: string) : Update =
    line.Split(",") |> Array.map int |> Array.toList

let rules: Rules = file[0].Split("\n") |> Array.map parseOrderingRule |> Array.toList
let updates: Updates = file[1].Split("\n") |> Array.map parseUpdate |> Array.toList

let comparePageNumbers(p1: PageNumber)(p2: PageNumber): int = 
    0

let validUpdate(original: Update): bool = 
    let sorted = original |> List.sortWith comparePageNumbers
    original = sorted

let validUpdates(u: Updates): Updates =
    u |> List.filter validUpdate

let finalUpdates = validUpdates updates