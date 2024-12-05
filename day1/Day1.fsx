let lines = [ for line in System.IO.File.ReadLines("input.txt") -> line ]

let pairs = lines |> List.map (fun x -> x.Split("   "))

let columnA = pairs |> List.map (fun x -> int x[0]) |> List.sort
let columnB = pairs |> List.map (fun x -> int x[1]) |> List.sort

let distance x y = 
    let sortedPair = [x; y] |> List.sort
    sortedPair[1] - sortedPair[0]

let distances = [ for (colA, colB) in List.zip columnA columnB -> distance colA  colB  ]

let sumDistance = List.sum distances

printfn "%d" sumDistance