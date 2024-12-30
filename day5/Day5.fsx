type PageNumber = int
type OrderingRule = {Before: PageNumber; After: PageNumber}
type Rules = OrderingRule list
type Update = PageNumber list
type PageNumbers = PageNumber list
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

// for debugging
let singleUpdates = [updates[0]]

let befores(p: PageNumber): PageNumbers =
    ([], rules) ||> List.fold(fun s v -> if v.After = p then v.Before :: s else s)
let afters(p: PageNumber): PageNumbers =
    ([], rules) ||> List.fold(fun s v -> if v.Before = p then v.After :: s else s)

// this may not be a sorting problem after all...
let comparePageNumbers(x: PageNumber)(y: PageNumber): int = 
    let xLowerThanY = -1
    let xHigherThanY = 1
    let theSame = 0

    let beforesX = befores x
    let aftersX = afters x
    let beforesY = befores y
    let aftersY = afters y
    let result = theSame
    printfn "x:%d goes before:%A but after: %A)\ny:%d goes before:%A but after: %A\nresult: %d\n" x beforesX aftersX y beforesY aftersY result
    result
    

let validUpdate(original: Update): bool = 
    let sorted = original |> List.sortWith comparePageNumbers
    original = sorted

let validUpdates(u: Updates): Updates =
    u |> List.filter validUpdate

let finalUpdates = validUpdates singleUpdates