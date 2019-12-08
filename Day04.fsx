open System.IO

let input =
    let text = File.ReadAllText @"./InputFiles/day04.txt"
    let values = text.Split('-') |> Array.map int
    values.[0], values.[1]

let chunkByRepeats (list: 'a list) =
    List.fold (fun (accum: 'a list list) (x: 'a) ->
        match accum with
        | head :: tail ->
            if (List.head head) = x then (x :: head) :: tail
            else [x] :: (head :: tail)
        | [] -> [[x]]

    ) [] list

let isNonDecreasing digits =
    digits
    |> List.pairwise
    |> List.forall (fun (x,y) -> x <= y)

// repeated adjacent elements of 2
let hasTwoAdjacentRepeat digits =
    digits
    |> List.pairwise
    |> List.exists (fun (x,y) -> x = y)

// repeated adjacent elements of 2 exact
let hasTwoAdjacentRepeatExact digits =
    digits
    |> chunkByRepeats
    |> List.exists (fun chunk -> List.length chunk = 2)

let part1() =
    let first, last = input
    seq [ first .. last ]
    |> Seq.map (string >> Seq.toList)
    |> Seq.filter isNonDecreasing
    |> Seq.filter hasTwoAdjacentRepeat
    |> Seq.length

let part2() =
    let first, last = input
    seq [ first .. last ]
    |> Seq.map (string >> Seq.toList)
    |> Seq.filter isNonDecreasing
    |> Seq.filter hasTwoAdjacentRepeatExact
    |> Seq.length