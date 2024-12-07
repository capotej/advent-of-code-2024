module Util
let unzipInputFileIntoLists =
    [ for line in System.IO.File.ReadLines("input.txt") -> line.Split("   ") ]
    // converts string array list to tuples, ["12","8"] -> (12,8) so we can unzip into separate list<int>
    |> List.map (fun x ->
        match x with
        | [| x; y |] -> (int x, int y)
        | _ -> failwith "Array must have exactly 2 elements"
    )
    |> List.unzip