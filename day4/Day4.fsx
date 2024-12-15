let lines =
    [ for line in System.IO.File.ReadLines("day4/input.txt") -> line]

// note: this is y,x format
let grid = lines |> List.map(fun x -> x |> Seq.toList)

// handles bounds
let lookup(y: int)(x: int): char =
    let y = if y < 0 then 0 else y
    let x = if x < 0 then 0 else x
    let y = if y > 139 then 139 else y
    let x = if x > 139 then 139 else x
    grid[y][x]

let all = List.allPairs [0..139] [0..139]

let starters =
    all
    |> List.filter (fun (pair: int * int) ->
        let y, x = pair
        (grid[y][x]).Equals 'X')

// we need a function that returns the list of `(int * int) list` of c found when found, rather than bool
// let adjacentChar (c: char) (pair: int * int): (int * int) = 
    // let y, x = pair
    // (lookup (y - 1) (x)).Equals(c) // 1
    // (lookup (y) (x - 1)).Equals(c) || // 2
    // (lookup (y - 1) (x - 1)).Equals(c) || // 3
    // (lookup (y + 1) (x)).Equals(c) || // 4
    // (lookup (y) (x + 1)).Equals(c) || // 5
    // (lookup (y + 1) (x + 1)).Equals(c) || // 6
    // (lookup (y + 1) (x - 1)).Equals(c) || // 7
    // (lookup (y - 1) (x + 1)).Equals(c)  // 8


let hasAdjacentChar (c: char) (pair: int * int): bool = 
    let y, x = pair
    (lookup (y - 1) (x)).Equals(c) || // 1
    (lookup (y) (x - 1)).Equals(c) || // 2
    (lookup (y - 1) (x - 1)).Equals(c) || // 3
    (lookup (y + 1) (x)).Equals(c) || // 4
    (lookup (y) (x + 1)).Equals(c) || // 5
    (lookup (y + 1) (x + 1)).Equals(c) || // 6
    (lookup (y + 1) (x - 1)).Equals(c) || // 7
    (lookup (y - 1) (x + 1)).Equals(c)  // 8


starters |> List.filter(hasAdjacentChar 'M')
         |> List.filter(hasAdjacentChar 'A') // starters is still the 'X', we need to filter on M coords...
         |> List.filter(hasAdjacentChar 'S')
         |> List.length
