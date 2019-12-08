// most of this code is copied from
// https://github.com/CameronAavik/AdventOfCode/blob/master/AdventOfCode.2019/Solutions/Day02.fs

open System.IO

let input =
    let text = File.ReadAllText @"./InputFiles/day02.txt"
    let values = text.Split(',') |> Array.toList |> List.map int
    let indices = [ 0 .. (List.length values) - 1 ]
    List.zip indices values |> Map.ofList

type State = { PC: int; IntaCode: Map<int, int>; halt: bool }
let get i state = state.IntaCode.[i]
let set i value state = { state with IntaCode = Map.add i value state.IntaCode }
let incPC state = { state with PC = state.PC + 4 }

let rec step state =
    match get state.PC state with
    | 1 | 2 as op ->
        let x = get (state.PC + 1) state
        let y = get (state.PC + 2) state
        let dest = get (state.PC + 3) state
        let op = if op = 1 then (+) else (*)
        set dest (op (get x state) (get y state)) state |> incPC
    | 99 -> { state with halt = true }
    | op -> failwithf "unknown opcode %d" op

let rec stepUntilHalt state =
    if state.halt then state
    else step state |> stepUntilHalt

let solve noun verb intaCode =
    let intaCode = intaCode |> Map.add 1 noun |> Map.add 2 verb
    let state = stepUntilHalt { PC = 0; IntaCode = intaCode; halt = false }
    get 0 state

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
