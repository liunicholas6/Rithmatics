module RithmaticsFs.Fit
open Godot

type RithmaticLineData =
    | WardingCircle of pos : Vector2 * r : float32
    | WardingEllipse of pos : Vector2 * dir : Vector2 * a : float32 * b : float32
    | Forbiddance of pos : Vector2 * dir : Vector2
    | Vigor of pos : Vector2 * dir : Vector2 * amp : float32 * omega : float32 * phi : float32
    | Revocation
    | Invalid

type Fit = {
    LineData : RithmaticLineData
    Error : float32
}

let deming (points : Vector2 array) =
    let centroid = Array.sum points / float32(points.Length)
    let rs = points |> Array.map(fun x -> x - centroid)
    let real =
        rs
        |> Array.map(fun v -> v.x * v.x - v.y * v.y)
        |> Array.sum
        |> sqrt
    let imaginary =
        rs
        |> Array.map(fun v -> -2f * v.x * v.y)
        |> Array.sum
        |> sqrt
    centroid, rs, Vector2(real, imaginary).Normalized()
let wardingCircle (points : Vector2 array) =
    let centroid = Array.sum points / float32(points.Length)
    let rsMag =
        points
        |> Array.map(fun x -> (x - centroid).Length())
    let radius = Array.average rsMag
    let err =
        rsMag
        |> Array.map(fun x -> (x - radius) ** 2f)
        |> Array.average
    Some {LineData = WardingCircle(centroid, radius); Error = err / radius}
    
let wardingEllipse (points : Vector2 array) =
    let centroid, rs, dir = deming points
    
    let axLength proj =
        rs
        |> Seq.fold(fun acc r ->
            let x = proj r
            (max x (fst acc), min x (snd acc))) (-infinityf, infinityf)
        |> fun (mx, mn) -> (mx - mn) / 2f
    
    let a = axLength (fun r -> r.Dot(dir))
    let b = axLength (fun r -> r.Cross(dir))
    let rPred theta =
        a * b / sqrt((b * Mathf.Cos theta) ** 2f + (a * Mathf.Sin theta) ** 2f)
    
    let err =
        rs
        |> Seq.map(fun r ->
            let theta = r.Angle() - dir.Angle()
            (r.Length() - rPred theta) ** 2f
            )
        |> Seq.average
    
    let lengthProxy =
        sqrt(a**2f + b**2f)
    
    Some {LineData = WardingEllipse(centroid, dir, a, b); Error = err/lengthProxy}

let forbiddance (points : Vector2 array) =
    let centroid, rs, dir = deming points
    let err =
        rs
        |> Seq.map (fun pt -> pt.Cross(dir) ** 2f)
        |> Seq.average
    let length = abs (rs[0].Dot(dir) - rs[-1].Dot(dir))
    Some {LineData = Forbiddance(centroid, dir); Error = err / length}
    
let vigor (points : Vector2 array) =
    //Use deming regression to estimate axis
    let centroid, rs, initDir = deming points
    
    //Binary search for direction that yields lower max projected distance
    let mutable dir  = initDir
    for dTheta in 0.08f |> Seq.unfold(fun x -> if x < 0.01f then None else Some(x, x/2f)) do
        dir <-
            seq {dTheta; -dTheta}
            |> Seq.map(dir.Rotated)
            |> Seq.minBy (fun dirGuess ->
                rs
                |> Seq.map(fun x -> abs(x.Dot(dirGuess)))
                |> Seq.max)
            
    //Flip direction to point towards first point drawn and set points relative to axis
    if rs[0].Dot(dir) < 0f then dir <- -dir
    let rs' = rs |> Array.map(fun x -> x.Rotated(-dir.Angle()))
    
    //Guess peaks and troughs
    let margin = 3
    let extrema =
        let averageY (v : Vector2 array) = v |> Array.averageBy(fun v -> v.y)
        let guesses = ResizeArray<Vector2>()
        seq {
            for chunk in rs' |> Seq.windowed(2 * margin + 1) do
                let point = chunk[margin]
                if (chunk[margin].y < averageY chunk[0 .. margin-1])
                       = (chunk[margin].y < averageY chunk[-margin .. -1]) then
                    guesses.Add(point)
                elif guesses.Count > 0 then
                    let extremum = guesses[guesses.Count/2]
                    yield guesses[guesses.Count/2]
                    guesses.Clear()
                }
        |> Seq.toArray
    
    if Seq.isEmpty extrema then None
    else
        
    //Guess other parameters
    let amp =
        extrema
        |> Seq.pairwise
        |> Seq.map(fun (u, v) -> abs(u.y - v.y))
        |> Seq.average
        
    
    let omega = float32(extrema.Length) / (extrema[0].x - extrema[-1].x) * Mathf.Pi
    
    //Two period check
    if (4f * Mathf.Pi/omega > rs'[0].x - rs'[-1].x) then None
    else
        
    let origin =
        extrema
        |> Seq.minBy(fun v -> abs(v.x - centroid.x))
    
    let phi =
        omega * origin.x |>
        fun x -> if origin.y > 0f then x + Mathf.Pi else x
        
    let err =
        rs'
        |> Seq.map(fun r -> (amp * Mathf.Cos(omega * r.x + phi) - r.y) ** 2f)
        |> Seq.average
        
    let scale =
        sqrt (amp * (rs'[0].x - rs'[-1].x))
    
    Some {LineData = Vigor(centroid, dir, amp, omega, phi); Error = err/scale}
    
