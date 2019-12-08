open System.IO

[<StructuredFormatDisplay("({x},{y})")>]
type Point = { x: int; y: int }

type Line = Point * Point

type LineType = | Horizontal | Vertical

type Stride =
    | Up of int
    | Down of int
    | Right of int
    | Left of int

let input =
    let parseStride (text: string) =
        let steps = int text.[1..]
        match text.[0] with
        | 'U' -> Up(steps)
        | 'D' -> Down(steps)
        | 'R' -> Right(steps)
        | 'L' -> Left(steps)
        | c -> failwithf "unknown direction %c" c

    let lines =
        @"./InputFiles/day03.txt"
        |> File.ReadAllLines
        |> Array.map (fun l ->
            l.Split(',')
            |> Array.map parseStride
            |> List.ofArray)

    lines.[0], lines.[1]

// intersection of two perpendicular lines
let intersection line1 line2 =
    let lineType line =
        let s, e = line
        if s.x = e.x then Vertical
        else Horizontal

    let isBetween n x y = (n >= x && n <= y) || (n >= y && n <= x)
    let x ({ x = x }, _) = x
    let y ({ y = y }, _) = y
    let x1 ({ x = x }, _) = x
    let x2 (_, { x = x }) = x
    let y1 ({ y = y }, _) = y
    let y2 (_, { y = y }) = y

    // line1 should Horizontal and line2 should be Vertical
    let intersection' line1 line2 =
        if not (isBetween (y line1) (y1 line2) (y2 line2)) then None
        elif not (isBetween (x line2) (x1 line1) (x2 line1)) then None
        else Some { x = (x line2); y = (y line1) }

    match (lineType line1), (lineType line2) with
    | (Horizontal, Vertical) -> intersection' line1 line2
    | (Vertical, Horizontal) -> intersection' line2 line1
    | _ -> None

let manhattan pos1 pos2 =
    abs (pos1.x - pos2.x) + abs (pos1.y - pos2.y)

let length line =
    let s, e = line
    if s.x = e.x then abs (s.y - e.y)
    else abs (s.x - e.x)

let moveByStride pos stride =
    match stride with
    | Up steps -> { pos with y = pos.y + steps }
    | Down steps -> { pos with y = pos.y - steps }
    | Right steps -> { pos with x = pos.x + steps }
    | Left steps -> { pos with x = pos.x - steps }

let stridesToLines =
    List.scan moveByStride { x = 0; y = 0 } >> List.pairwise

let linesToCumulativeSteps =
    List.scan (fun total line -> total + length line) 0
    >> List.rev
    >> List.tail
    >> List.rev

let part1() =
    let strides1, strides2 = input
    let wire1 = stridesToLines strides1
    let wire2 = stridesToLines strides2

    let intersections =
        seq {
            for line1 in wire1 do
                for line2 in wire2 do
                    let intersection = intersection line1 line2
                    if intersection.IsSome then intersection.Value
        } |> Seq.tail

    intersections
    |> Seq.map (manhattan { x = 0; y = 0 })
    |> Seq.min

let part2() =
    let strides1, strides2 = input

    let wire1 = stridesToLines strides1
    let wire2 = stridesToLines strides2

    let signalDelays1 = linesToCumulativeSteps wire1
    let signalDelays2 = linesToCumulativeSteps wire2

    let signalDelayAtIntersections =
        seq {
            for line1, delay1 in List.zip wire1 signalDelays1 do
                for line2, delay2 in List.zip wire2 signalDelays2 do
                    let intersection = intersection line1 line2
                    if intersection.IsSome then
                        let intersection = intersection.Value
                        let s1, _ = line1
                        let s2, _ = line2
                        let wire1Delay = delay1 + (length (s1, intersection))
                        let wire2Delay = delay2 + (length (s2, intersection))
                        wire1Delay + wire2Delay
        } |> Seq.tail

    signalDelayAtIntersections |> Seq.min