open System.IO

exception UnknownOpException of string

let input =
    let text = File.ReadAllText @"./InputFiles/day02.txt"
    text.Split(',') |> Array.map int

let part1() =
    Array.set input 1 12
    Array.set input 2 2
    let halt = ref false
    for i in [ 0 .. 4 .. Array.length input ] do
        if not halt.Value then halt := input.[i + 0] = 99
        if not halt.Value then
            let op =
                match input.[i + 0] with
                | 1 -> (+)
                | 2 -> (*)
                | op -> raise (UnknownOpException(sprintf "unknown operator %d" op))

            Array.set input input.[i + 3] (op input.[input.[i + 1]] input.[input.[i + 2]])

let part2() = 5