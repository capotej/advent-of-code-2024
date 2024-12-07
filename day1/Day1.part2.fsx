#load "Util.fsx"

let left, right = Util.unzipInputFileIntoLists

let timesNumberWasFound (number: int) (list: int list) : int =
    (0, list) ||> List.fold (fun acc n -> if n = number then acc + 1 else acc)

let similarity =
    left
    |> List.map (fun leftNumber -> leftNumber * timesNumberWasFound leftNumber right)
    |> List.sum

printfn "%d" similarity 
