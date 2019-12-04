open System.IO

let input =
    @"./InputFiles/day01.txt"
    |> File.ReadAllLines
    |> Array.toList
    |> List.map int

let calculateFuel1 mass = (mass / 3) - 2

let rec calculateFuel2 mass =
    let fuel = (mass / 3) - 2
    if fuel > 0 then fuel + calculateFuel2 fuel
    else 0

let part1() = input |> List.sumBy calculateFuel1

let part2() = input |> List.sumBy calculateFuel2
