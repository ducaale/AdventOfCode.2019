
#load "../Day01.fsx"
open Day01

let ``calculateFuel1: mass of 12`` = (calculateFuel1 12) = 2
let ``calculateFuel1: mass of 14`` = (calculateFuel1 14) = 2
let ``calculateFuel1: mass of 1969`` = (calculateFuel1 1969) = 654
let ``calculateFuel1: mass of 100756`` = (calculateFuel1 100756) = 33583

let ``calculateFuel2: mass of 14`` = (calculateFuel2 14) = 2
let ``calculateFuel2: mass of 1969`` = (calculateFuel2 1969) = 966
let ``calculateFuel2: mass of 100756`` = (calculateFuel2 100756) = 50346