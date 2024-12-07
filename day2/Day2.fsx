

let isAllIncreasing (list: int array): bool = 
    Array.sort(list) = list
let isAllDecreasing (list: int array): bool = 
    Array.sortDescending(list) = list

let isIncreasingOrDecreasing(list: int array): bool =
    isAllIncreasing(list) || isAllDecreasing(list)

let withinRange (x:int) (y:int): bool =
    let result = abs (x - y) 
    result <= 3 && result >= 1

let isOnlyIncreasingOrDecreasingByOneOrTwo(list: int array): bool =
    list |> Array.pairwise |> Array.forall(fun (x,y) -> withinRange x y)

let allReports =
    [ for line in System.IO.File.ReadLines("input.txt") -> line.Split(" ") |> Array.map(fun x -> int x) ]

let isSafeReport (list: int array): bool = 
    isIncreasingOrDecreasing(list) && isOnlyIncreasingOrDecreasingByOneOrTwo(list)

let safeReports = allReports |> List.filter(fun x -> isSafeReport x)

printfn "%d" (List.length safeReports)
