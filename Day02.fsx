open System.IO

#load "IntaComputer.fsx"
open IntaComputer

let input =
    let text = File.ReadAllText @"./InputFiles/day02.txt"
    let values = text.Split(',') |> Array.toList |> List.map int
    let indices = [ 0 .. (List.length values) - 1 ]
    List.zip indices values |> Map.ofList

let solve noun verb intaCode =
    let intaCode = intaCode |> Map.add 1 noun |> Map.add 2 verb
    let state =
        stepUntilHalt { PC = 0
                        intaCode = intaCode
                        halt = false
                        inputStack = []
                        outputStack = [] }

    get 0 Immediate state

let part1() =
    let intaCode = input
    solve 12 2 intaCode

let part2() =
    let intaCode = input
    seq {
        for noun in [0..99] do
            for verb in [0..99] do
                if solve noun verb intaCode = 19690720 then
                    100 * noun  + verb
    } |> Seq.head
