type Coordinate = {X:int; Y:int; Data:char}
type Grid = Coordinate list
type Word = Coordinate seq

let loadGridFromFile(file: string): Grid = 
    let lines = [ for line in System.IO.File.ReadLines(file) -> line]
    let dArray = lines |> List.map(fun x -> x |> Seq.toList)
    // note: this is y,x format
    let coords = dArray |> List.mapi (fun x l -> (l |> List.mapi (fun y c -> { Data = c; X = y; Y = x })))
    coords |> List.collect(fun x -> x)

let grid = loadGridFromFile "day4/testinput.txt"

let gridContainsCoord(coord: Coordinate): bool =
    grid |> List.exists(fun x -> x = coord)

let gridContainsWord(w: Word): bool = 
    w |> Seq.forall(fun x -> gridContainsCoord x)

let candidates(c: Coordinate): Word list =
    let y, x = c.Y, c.X

    [ [ { Y = y - 1; X = x; Data = 'M' }
        { Y = y - 2; X = x; Data = 'A' }
        { Y = y - 3; X = x; Data = 'S' } ] |> List.toSeq // 1
      [ { Y = y; X = x - 1; Data = 'M' }
        { Y = y; X = x - 2; Data = 'A' }
        { Y = y; X = x - 3; Data = 'S' } ] |> List.toSeq // 2
      [ { Y = y - 1; X = x - 1; Data = 'M' }
        { Y = y - 2; X = x - 2; Data = 'A' }
        { Y = y - 3; X = x - 3; Data = 'S' } ] |> List.toSeq // 3
      [ { Y = y + 1; X = x; Data = 'M' }
        { Y = y + 2; X = x; Data = 'A' }
        { Y = y + 3; X = x; Data = 'S' } ] |> List.toSeq // 4
      [ { Y = y; X = x + 1; Data = 'M' }
        { Y = y; X = x + 2; Data = 'A' }
        { Y = y; X = x + 3; Data = 'S' } ] |> List.toSeq // 5
      [ { Y = y + 1; X = x + 1; Data = 'M' }
        { Y = y + 2; X = x + 2; Data = 'A' }
        { Y = y + 3; X = x + 3; Data = 'S' } ] |> List.toSeq // 6
      [ { Y = y + 1; X = x - 1; Data = 'M' }
        { Y = y + 2; X = x - 2; Data = 'A' }
        { Y = x + 3; X = x - 3; Data = 'S' } ] |> List.toSeq // 7
      [ { Y = y - 1; X = x + 1; Data = 'M' }
        { Y = y - 2; X = x + 2; Data = 'A' }
        { Y = y - 3; X = x + 3; Data = 'S' } ] |> List.toSeq // 8 
    ] 

let wordsFoundForCoord(c: Coordinate): int =
    // search all directions
    (0, (candidates c)) ||> List.fold(fun s w -> if gridContainsWord w then s + 1 else s)

let result =
    grid
    |> List.filter (fun x -> x.Data = 'X')
    |> List.map wordsFoundForCoord
    |> List.sum






