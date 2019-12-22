open System.IO

#load "IntaComputer.fsx"
open IntaComputer

let input =
    let text = File.ReadAllText @"./InputFiles/day05.txt"
    let values = text.Split(',') |> Array.toList |> List.map int
    let indices = [ 0 .. (List.length values) - 1 ]
    List.zip indices values |> Map.ofList

let solve input intaCode =
    let state =
        stepUntilHalt { PC = 0
                        intaCode = intaCode
                        halt = false
                        inputStack = [input]
                        outputStack = [] }

    let output, _ = popOutput state
    output

let part1() =
    let intaCode = input
    solve 1 intaCode

let part2() =
    let intaCode = input
    solve 5 intaCode