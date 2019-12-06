open System.IO


[<StructuredFormatDisplay("({x},{y})")>]
type Coordinate = { x: int; y: int }

type Line = Coordinate * Coordinate

type LineType = | Horizontal | Vertical

type Movement =
    | Up of int
    | Down of int
    | Right of int
    | Left of int

let input = 
    let commandToMovement (command: string) =
        let movement = int command.[1..]
        match command.[0] with
        | 'U' -> Up(movement)
        | 'D' -> Down(movement)
        | 'R' -> Right(movement)
        | 'L' -> Left(movement)
        | c -> failwithf "unknown command %c" c

    let lines = @"./InputFiles/day03.txt"
                |> File.ReadAllLines
                |> Array.map (fun l ->
                    l.Split(',') |> Array.map commandToMovement |> List.ofArray
                )
    (lines.[0], lines.[1])

let lineType line = 
    let (s, e) = line
    if s.x = e.x then Vertical else Horizontal

// intersection of two perpendicular lines
let intersection line1 line2 =
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

    match ((lineType line1), (lineType line2)) with
    | (Horizontal, Vertical) -> intersection' line1 line2
    | (Vertical, Horizontal) -> intersection' line2 line1
    | _ -> None

let manhattanFromOrigin pos = (abs pos.x) + (abs pos.y)

let advance position movement =
    match movement with
    | Up steps -> { position with y = position.y + steps }
    | Down steps -> { position with y = position.y - steps }
    | Right steps -> { position with x = position.x + steps }
    | Left steps -> { position with x = position.x - steps }

let rec pathToCoordinates coordinates path =
    if List.isEmpty coordinates then failwith "coordinates shouldn't be empty"
    if List.isEmpty path then
        coordinates
    else
        let pos = List.head coordinates
        let coordinates = (advance pos (List.head path)) :: coordinates
        pathToCoordinates coordinates (List.tail path)

let rec coordinatesToLines lines coordinates =
    if (List.length coordinates) < 2 then lines
    else
        let line = (coordinates.[1], coordinates.[0])
        let lines = line :: lines
        coordinatesToLines lines (List.tail coordinates)

let lines path =
    let initialPos = { x = 0; y = 0 }
    path |> pathToCoordinates [ initialPos ] |> coordinatesToLines []

let part1() =
    let path1, path2 = input
    let wire1 = lines path1
    let wire2 = lines path2

    let intersections = seq {
        for line1 in wire1 do
            for line2 in wire2 do
                let intersection = intersection line1 line2
                if intersection.IsSome then intersection.Value }
    
    // assume that intersection at (0,0) occurs only once
    intersections |> Seq.tail |> Seq.map manhattanFromOrigin |> Seq.min