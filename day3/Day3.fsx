open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllBytes("input.txt") |> System.Text.Encoding.ASCII.GetString

let matches = Regex.Matches(input, @"mul\((\d+),(\d+)\)")

// [ for foundMatch in matches ->  printfn "%A" foundMatch.Groups ] |> Seq.map
let results =
    [ for foundMatch in matches -> foundMatch ]
    |> List.map (fun x -> int x.Groups[1].Value * int x.Groups[2].Value)

printfn "%d" (results |> List.sum)


