open System.IO

#load "IntaComputer.fsx"
open IntaComputer

let input() =
  let text = File.ReadAllText @"./InputFiles/day07.txt"
  let values = text.Split(',') |> Array.toList |> List.map int
  let indices = [ 0 .. (List.length values) - 1 ]
  List.zip indices values |> Map.ofList

let permute list =
  let rec inserts e = function
    | [] -> [[e]]
    | x::xs as list -> (e::list)::(inserts e xs |> List.map (function xs' -> x::xs'))

  List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

let configAmplifier intaCode phaseSetting =
  { PC = 0
    intaCode = intaCode
    halt = false
    inputQueue = [phaseSetting]
    outputStack = [] }

let amplify signal amplifiers =
  let rec amplify' signal amplifierStates = function
    | [] -> signal, amplifierStates
    | x::xs ->
      let x = enqueInput signal x |> stepUntilOutputOrHalt
      let signal, state = popOutput x
      amplify' signal (state::amplifierStates) xs

  amplify' signal [] amplifiers

let rec amplifyUntilHalt signal amplifiers =
  let signal, states = amplify signal amplifiers
  let lastAmplifier = List.head states |> stepUntilInputOrHalt
  if lastAmplifier.halt then signal, states
  else amplifyUntilHalt signal (List.rev states)

let part1() =
  let intaCode = input()
  seq {
    for settings in permute [0..4] do
      let amplifiers = List.map (fun phase -> configAmplifier intaCode phase) settings
      let signal, _ = amplify 0 amplifiers
      signal

  } |> Seq.max

let part2() =
  let intaCode = input()
  seq {
    for settings in permute [5..9] do
      let amplifiers = List.map (fun phase -> configAmplifier intaCode phase) settings
      let signal, _ = amplifyUntilHalt 0 amplifiers
      signal

  } |> Seq.max