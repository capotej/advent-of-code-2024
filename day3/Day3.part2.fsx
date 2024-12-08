open System.Text.RegularExpressions

let rawInput =
    System.IO.File.ReadAllBytes("input.txt") |> System.Text.Encoding.ASCII.GetString

let filteredInput =
    rawInput.Split("do()") |> Array.map (fun x -> x.Split("don't()")[0]) |> String.concat ""

let matches = Regex.Matches(filteredInput, @"mul\((\d+),(\d+)\)")

let results =
    [ for foundMatch in matches -> foundMatch ]
    |> List.map (fun x -> int x.Groups[1].Value * int x.Groups[2].Value)

printfn "%d" (results |> List.sum)


