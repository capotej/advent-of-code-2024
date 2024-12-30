type PageNumber = int
type OrderingRule = {Before: PageNumber; After: PageNumber}
type Rules = OrderingRule list
type Update = PageNumber list
type PageNumbers = PageNumber list
type Updates = Update list
let file = System.IO.File.ReadAllText("day5/input.txt").Split("\n\n")
let parseOrderingRule (pair: string) : OrderingRule =
    let parts = pair.Split("|")
    let before = parts[0] |> int
    let after = parts[1] |> int
    { Before = before; After = after }

let parseUpdate (line: string) : Update =
    line.Split(",") |> Array.map int |> Array.toList

let rules: Rules = file[0].Split("\n") |> Array.map parseOrderingRule |> Array.toList
let updates: Updates = file[1].Split("\n") |> Array.map parseUpdate |> Array.toList

let pagesBeforeTarget(target: PageNumber): PageNumbers =
    ([], rules) ||> List.fold(fun s v -> if v.After = target then v.Before :: s else s)
let pagesAfterTarget(target: PageNumber): PageNumbers =
    ([], rules) ||> List.fold(fun s v -> if v.Before = target then v.After :: s else s)

type Organizer(u: Update, r: Rules) =
    let mutable correct: bool option = None
    let mutable mutated: bool = false
    let mutable internalPages: Update = u
    member this.Mutated
        with get () = mutated
        and set (value) = mutated <- value
    member this.Correct
        with get () = correct
        and set (value) = correct <- value
    member this.Pages
        with get () = internalPages
        and set (value) = internalPages <- value
    member this.IndexFor(p: PageNumber): int option = 
        this.Pages |> List.tryFindIndex(fun x -> x = p)
    member this.Rules = r
    member this.InsertBeforeTarget(target: PageNumber)(p: PageNumber): unit = 
        this.Mutated <- true
        let tIdx = this.IndexFor target
        if tIdx.IsSome then
            this.Pages <- this.Pages |> List.insertAt (tIdx.Value - 1) p
        ()
    member this.InsertAfterTarget(target: PageNumber)(p: PageNumber): unit = 
        this.Mutated <- true
        let tIdx = this.IndexFor target
        if tIdx.IsSome then
            this.Pages <- this.Pages |> List.insertAt (tIdx.Value + 1) p
        () 
    member this.IsTargetBefore(target: PageNumber)(p: PageNumber): bool option = 
        match this.IndexFor target, this.IndexFor p with
        | Some(t),Some(p) -> Some(t < p)
        | None,_ -> None
        | _, None -> None
    
    member this.IsTargetAfter(target: PageNumber)(p: PageNumber): bool option = 
        match this.IndexFor target, this.IndexFor p with
        | Some(t),Some(p) -> Some(t > p)
        | None,_ -> None
        | _, None -> None

    member this.Organize: unit = 
        this.Pages |> List.iter (fun target -> 
            pagesAfterTarget target |> List.iter(fun after -> 
                let result = this.IsTargetAfter target after
                if result.IsSome && result.Value = true then this.Correct <- Some(false)
                printfn "%A after %A = %A" target after result
                ()
            )
            pagesBeforeTarget target |> List.iter(fun before -> 
                let result = this.IsTargetBefore target before
                if result.IsSome && result.Value = true then this.Correct <- Some(false)
                printfn "%A before %A = %A" target before result
                ()
            )
            if this.Correct <> Some(false) then this.Correct <- Some(true)
            ())


let validUpdate(u: Update): bool =
    let og = Organizer(u, rules)
    og.Organize
    og.Correct.IsSome && og.Correct.Value = true 

exception ListNotOdd of string
let middleOfList(l: 'a list): 'a = 
    if (List.length l) % 2 = 0 then raise (ListNotOdd("list not odd"))
    l |> List.item (List.length l / 2)

let sumOfMiddle(u: Updates): int = 
    (0, u) ||> List.fold(fun s v -> (middleOfList v) + s)
let correct = updates |> List.filter validUpdate |> sumOfMiddle

