#load "Util.fsx"

let left, right = Util.unzipInputFileIntoLists

let distance x y =
    let sortedPair = [ x; y ] |> List.sort
    sortedPair[1] - sortedPair[0]

let sumDistance = [ for (colA, colB) in List.zip left right -> distance colA colB  ] |> List.sum

printfn "%d" sumDistance