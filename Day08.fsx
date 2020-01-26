open System
open System.IO

type LayerStats = { zeros: int; ones: int; twos: int } 

let width, height = 25, 6
let input() = 
  File.ReadAllText @"./InputFiles/day08.txt"
  |> Seq.chunkBySize (width * height)
  |> Seq.map Seq.ofArray

let getStats layer =
    layer
    |> Seq.countBy id
    |> Map.ofSeq
    |> (fun stats ->
        { zeros = stats.['0']
          ones = stats.['1']
          twos = stats.['2'] })

let overlay layer1 layer2 =
  Seq.zip layer1 layer2
  |> Seq.map (fun (d1, d2) -> if d1 = '2' then d2 else d1)

let toImage line =
  Seq.map (fun char -> if char = '1' then '1' else ' ') line

let part1() =
    input()
    |> Seq.map getStats
    |> Seq.minBy (fun layerStats -> layerStats.zeros)
    |> (fun layerStats -> layerStats.ones * layerStats.twos)

let part2() =
  let image = input() |> Seq.reduce overlay
  printf "\n== Beginning of transmission\n\n"
  image
  |> Seq.chunkBySize width
  |> (Seq.map (Seq.ofArray >> toImage >> String.Concat))
  |> Seq.iter (fun line -> printfn "%s" line)
  printf "\n== End of transmission\n\n"
  ()