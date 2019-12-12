open System.IO

let input =
    let text = File.ReadAllText @"./InputFiles/day05.txt"
    let values = text.Split(',') |> Array.toList |> List.map int
    let indices = [ 0 .. (List.length values) - 1 ]
    List.zip indices values |> Map.ofList

type State = { PC: int; intaCode: Map<int, int>; input: int; output: int option; halt: bool }

type AccessMode = 
    | Immediate
    | Position

type Opcode =
    | Add
    | Mul
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | Halt

let incPC amount state = { state with PC = state.PC + amount }
let setPC value state = { state with PC = value }
let set i value state = { state with intaCode = Map.add i value state.intaCode }
let setOutput value state = { state with output = Some value }
let rec get i mode state =
    match mode with
    | Immediate -> state.intaCode.[i]
    | Position -> get state.intaCode.[i] Immediate state

let parseOp op =
    let op = sprintf "%04d" op
    let yMode = if op.[0] = '0' then Position else Immediate
    let xMode = if op.[1] = '0' then Position else Immediate
    let op =
        match int op.[2..3] with
        | 01 -> Add
        | 02 -> Mul
        | 03 -> Input
        | 04 -> Output
        | 05 -> JumpIfTrue
        | 06 -> JumpIfFalse
        | 07 -> LessThan
        | 08 -> Equals
        | 99 -> Halt
        | op -> failwithf "unknown opcode %d" op
    op, xMode, yMode

let rec step state =
    let op, xMode, yMode = parseOp (get state.PC Immediate state)
    match op with
    | Add | Mul as op ->
        let x = get (state.PC + 1) xMode state
        let y = get (state.PC + 2) yMode state
        let dest = get (state.PC + 3) Immediate state
        let op = if op = Add then (+) else (*)
        set dest (op x y) state |> incPC 4
    | Input ->
        let dest = get (state.PC + 1) Immediate state
        set dest state.input state |> incPC 2
    | Output ->
        let x = get (state.PC + 1) Position state
        printf "output %d\n" x
        setOutput x state |> incPC 2
    | JumpIfTrue ->
        let x = get (state.PC + 1) xMode state
        let y = get (state.PC + 2) yMode state
        if x <> 0 then setPC y state
        else state |> incPC 3
    | JumpIfFalse ->
        let x = get (state.PC + 1) xMode state
        let y = get (state.PC + 2) yMode state
        if x = 0 then setPC y state
        else state |> incPC 3
    | LessThan ->
        let x = get (state.PC + 1) xMode state
        let y = get (state.PC + 2) yMode state
        let dest = get (state.PC + 3) Immediate state
        let state =
            if x < y then set dest 1 state
            else set dest 0 state
        state |> incPC 4
    | Equals ->
        let x = get (state.PC + 1) xMode state
        let y = get (state.PC + 2) yMode state
        let dest = get (state.PC + 3) Immediate state
        let state =
            if x = y then set dest 1 state
            else set dest 0 state
        state |> incPC 4
    | Halt -> { state with halt = true }

let rec stepUntilHalt state =
    if state.halt then state
    else step state |> stepUntilHalt

let solve input intaCode =
    let state =
        stepUntilHalt { PC = 0
                        intaCode = intaCode
                        halt = false
                        input = input
                        output = None }
    state.output

let part1() =
    let intaCode = input
    solve 1 intaCode

let part2() =
    let intaCode = input
    solve 5 intaCode