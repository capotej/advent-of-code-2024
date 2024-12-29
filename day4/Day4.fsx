type Coordinate = {X:int; Y:int; Data:char}
type Grid = Coordinate list
type Word = Coordinate seq

let loadGridFromFile(file: string): Grid = 
    let lines = [ for line in System.IO.File.ReadLines(file) -> line]
    let dArray = lines |> List.map(fun x -> x |> Seq.toList)
    // note: this is y,x format
    let coords = dArray |> List.mapi (fun x l -> (l |> List.mapi (fun y c -> { Data = c; X = y; Y = x })))
    coords |> List.collect(fun x -> x)

let grid = loadGridFromFile "day4/input.txt"

let gridContainsCoord(coord: Coordinate): bool =
    grid |> List.exists(fun x -> x = coord)

let gridContainsWord(w: Word): bool = 
    w |> Seq.forall(fun x -> gridContainsCoord x)

// XMAS in all 8 directions
let XMASCandidates(c: Coordinate): Word list =
    let y, x = c.Y, c.X

    [ [ { Y = y; X = x; Data = 'X'} 
        { Y = y - 1; X = x; Data = 'M' }
        { Y = y - 2; X = x; Data = 'A' }
        { Y = y - 3; X = x; Data = 'S' } ] // 1

      [ { Y = y; X = x; Data = 'X'}
        { Y = y; X = x - 1; Data = 'M' }
        { Y = y; X = x - 2; Data = 'A' }
        { Y = y; X = x - 3; Data = 'S' } ] // 2

      [ { Y = y; X = x; Data = 'X'}
        { Y = y - 1; X = x - 1; Data = 'M' }
        { Y = y - 2; X = x - 2; Data = 'A' }
        { Y = y - 3; X = x - 3; Data = 'S' } ] // 3

      [ { Y = y; X = x; Data = 'X'}
        { Y = y + 1; X = x; Data = 'M' }
        { Y = y + 2; X = x; Data = 'A' }
        { Y = y + 3; X = x; Data = 'S' } ]  // 4

      [ { Y = y; X = x; Data = 'X'}
        { Y = y; X = x + 1; Data = 'M' }
        { Y = y; X = x + 2; Data = 'A' }
        { Y = y; X = x + 3; Data = 'S' } ] // 5

      [ { Y = y; X = x; Data = 'X'}
        { Y = y + 1; X = x + 1; Data = 'M' }
        { Y = y + 2; X = x + 2; Data = 'A' }
        { Y = y + 3; X = x + 3; Data = 'S' } ] // 6

      [ { Y = y; X = x; Data = 'X'}
        { Y = y + 1; X = x - 1; Data = 'M' }
        { Y = y + 2; X = x - 2; Data = 'A' }
        { Y = y + 3; X = x - 3; Data = 'S' } ]  // 7

      [ { Y = y; X = x; Data = 'X'}
        { Y = y - 1; X = x + 1; Data = 'M' }
        { Y = y - 2; X = x + 2; Data = 'A' }
        { Y = y - 3; X = x + 3; Data = 'S' } ] // 8 
    ] 

let xMASCandidates(c: Coordinate): Word list =
    let y, x = c.Y, c.X
    [ [ { Y = y; X = x; Data = 'M'} 
        { Y = y; X = x + 2; Data = 'M' }
        { Y = y + 1; X = x + 1; Data = 'A' }
        { Y = y + 2; X = x; Data = 'S' } 
        { Y = y + 2; X = x + 2; Data = 'S' } ] // 1
      [ { Y = y; X = x; Data = 'M'} 
        { Y = y + 1; X = x + 1; Data = 'A' }
        { Y = y + 2; X = x; Data = 'M' }
        { Y = y; X = x + 2; Data = 'S' }
        { Y = y + 2; X = x + 2; Data = 'S' } ] // 2
      [ { Y = y; X = x; Data = 'S'}
        { Y = y + 1; X = x + 1; Data = 'A' }
        { Y = y + 2; X = x; Data = 'M' }
        { Y = y; X = x + 2; Data = 'S' }
        { Y = y + 2; X = x + 2; Data = 'M' } ] // 3
      [ { Y = y; X = x; Data = 'S'} 
        { Y = y + 1; X = x + 1; Data = 'A' }
        { Y = y + 2; X = x; Data = 'S' }
        { Y = y; X = x + 2; Data = 'M' }
        { Y = y + 2; X = x + 2; Data = 'M' } ] // 4
    ]
    

// Part 1
let xmasFoundForCoord(c: Coordinate): int =
    (0, (XMASCandidates c)) ||> List.fold(fun s w -> if gridContainsWord w then s + 1 else s)

// Part 2
let masFoundForCoord(c: Coordinate): int =
    (0, (xMASCandidates c)) ||> List.fold(fun s w -> if gridContainsWord w then s + 1 else s)


let masResult =
    grid
    |> List.map masFoundForCoord
    |> List.sum
