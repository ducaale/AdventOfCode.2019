open System.IO

#load "IntaComputer.fsx"
open IntaComputer

let input() =
    let text = File.ReadAllText @"./InputFiles/day07.txt"
    let values = text.Split(',') |> Array.toList |> List.map int
    let indices = [ 0 .. (List.length values) - 1 ]
    List.zip indices values |> Map.ofList

let amplify phaseSetting inputSignal =
    let intaCode = input()
    let state = { PC = 0
                  intaCode = intaCode
                  halt = false
                  inputStack = [phaseSetting; inputSignal]
                  outputStack = [] }

    let output, _ = state |> stepUntilHalt |> popOutput
    output

let permute list =
  let rec inserts e list =
    match list with
    | [] -> [[e]]
    | x::xs -> (e::list)::(inserts e xs |> List.map (function xs' -> x::xs'))

  List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

let part1() =
    let possibleSettings = permute [0; 1; 2; 3; 4]
    let configAmplifier phase = amplify phase

    seq {
        for setting in possibleSettings do
            setting
            |> List.map configAmplifier
            |> List.fold (fun signal amplify -> amplify signal) 0
    } |> Seq.max