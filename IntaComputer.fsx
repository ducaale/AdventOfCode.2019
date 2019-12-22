type State = { PC: int
               intaCode: Map<int, int>
               inputStack: int list
               outputStack: int list
               halt: bool }

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

let pushOutput value state = { state with outputStack = value::state.outputStack }
let popOutput state =
    match state.outputStack with
    | [] -> failwith "output is empty"
    | x::xs -> x, { state with outputStack = xs }

let pushInput value state = { state with inputStack = value::state.inputStack }
let popInput state =
    match state.inputStack with
    | [] -> failwith "input is empty"
    | x::xs -> x, { state with inputStack = xs }

let set i value state = { state with intaCode = Map.add i value state.intaCode }
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
        let input, state = popInput state
        set dest input state |> incPC 2
    | Output ->
        let x = get (state.PC + 1) Position state
        pushOutput x state |> incPC 2
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