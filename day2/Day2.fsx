
let isAllIncreasing (list: int array): bool = 
    Array.sort(list) = list
let isAllDecreasing (list: int array): bool = 
    Array.sortDescending(list) = list

let isIncreasingOrDecreasing(list: int array): bool =
    isAllIncreasing(list) || isAllDecreasing(list)

let isWithinRange (x:int) (y:int): bool =
    let result = abs (x - y) 
    result <= 3 && result >= 1

let listWithinRange(list: int array): bool =
    list |> Array.pairwise |> Array.forall(fun (x,y) -> isWithinRange x y)

let allReports =
    [ for line in System.IO.File.ReadLines("input.txt") -> line.Split(" ") |> Array.map(fun x -> int x) ]

let isSafeReport (list: int array): bool = 
    isIncreasingOrDecreasing(list) && listWithinRange(list)

let isPossiblySafe(list: int array) = 
    let permutedLists = list |> Array.mapi(fun i x -> Array.removeAt i list)
    permutedLists |> Array.exists(fun x -> isSafeReport x)

let safeReports, unsafeReports = allReports |> List.partition(fun x -> isSafeReport x)
let possiblySafeReports = unsafeReports |> List.filter(fun x -> isPossiblySafe(x))


printfn "absolutely safe reports: %d" (List.length safeReports)
printfn "possibly safe reports: %d" (List.length possiblySafeReports)
printfn "total safe: %d" (List.length safeReports + List.length possiblySafeReports)
