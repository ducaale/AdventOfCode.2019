open System.IO

let input =
    @"./InputFiles/day06.txt"
    |> File.ReadAllLines
    |> Array.map (fun l ->
        let p = l.Split(')')
        p.[1], p.[0]
    )
    |> Map.ofArray

let rec orbitDistanceToCOM (orbits: Map<string, string>) (planet: string) =
    if planet = "COM" then 0
    else 1 + orbitDistanceToCOM orbits orbits.[planet]

let rec pathToCOM (orbits: Map<string, string>) (planet: string) =
    if planet = "COM" then [ planet ]
    else planet :: (pathToCOM orbits orbits.[planet])

let part1() =
    let orbits = input
    orbits
    |> Map.toSeq
    |> Seq.sumBy (fun (k, _) -> orbitDistanceToCOM orbits k)

let part2() =
    let orbits = input
    let a = Set.ofList (pathToCOM orbits "SAN")
    let b = Set.ofList (pathToCOM orbits "YOU")
    (a - b) + (b - a) |> Set.count |> (+) -2