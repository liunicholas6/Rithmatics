module RithmaticsFs.Fitting
open System.Numerics
open Godot
type RithmaticLineData =
    | WardingCircle of pos : Vector2 * r : float32
    | WardingEllipse of pos : Vector2 * dir : Vector2 * a : float32 * b : float32
    | Forbiddance of p1: Vector2 * p2 : Vector2
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
        |> Seq.map(fun v -> v.x * v.x - v.y * v.y)
        |> Seq.sum
    let imaginary =
        rs
        |> Seq.map(fun v -> v.x * v.y)
        |> Seq.sum
        |> (*) 2f
    let z = Complex(float(real), float(imaginary)) |> sqrt
    centroid, rs, Vector2(float32(z.Real), float32(z.Imaginary)).Normalized()
                   
let wardingCircle (points : Vector2 array) =
    let centroid = Array.sum points / float32(points.Length)
    let rsMag =
        points
        |> Array.map(fun x -> (x - centroid).Length())
    let radius = Array.average rsMag
    let err =
        rsMag
        |> Seq.map(fun x -> (x - radius) ** 2f)
        |> Seq.average
    {LineData = WardingCircle(centroid, radius); Error = err / radius}

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
    
    {LineData = WardingEllipse(centroid, dir, a, b); Error = err/lengthProxy}

let forbiddance (points : Vector2 array) =
    let points = points
    let centroid, rs, dir = deming points
    let err =
        rs
        |> Seq.map (fun pt -> pt.Cross(dir) ** 2f)
        |> Seq.average
    let p1 = rs[0].Project(dir) + centroid
    let p2 = rs[rs.Length-1].Project(dir) + centroid
    let length = (p1 - p2).Length()
    {LineData = Forbiddance(p1, p2); Error = err / length}




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
    do if rs[0].Dot(dir) < 0f then dir <- -dir
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
                       = (chunk[margin].y < averageY chunk[-margin .. chunk.Length - 1]) then
                    guesses.Add(point)
                elif guesses.Count > 0 then
                    yield guesses[guesses.Count/2]
                    guesses.Clear()
                }
        |> Seq.toArray
    
    if Seq.isEmpty extrema then
        {LineData = Invalid; Error = infinityf}
    else
        
    //Guess other parameters
    let amp =
        extrema
        |> Seq.pairwise
        |> Seq.map(fun (u, v) -> abs(u.y - v.y))
        |> Seq.average
    
    let omega = float32(extrema.Length) / (extrema[0].x - extrema[extrema.Length-1].x) * Mathf.Pi
    
    //Two period check
    if (4f * Mathf.Pi/omega < rs'[extrema.Length-1].x - rs'[0].x) then
        {LineData = Invalid; Error = infinityf}
    else
        
    //Guess the extrema closest to the centroid as the cosine origin
    let origin =
        extrema
        |> Seq.minBy(fun v -> abs(v.x - centroid.x))
    
    //Phase shift so that origin is on peak
    let phi =
        omega * origin.x
        |> fun x -> if origin.y > 0f then x + Mathf.Pi else x
        
    let err =
        rs'
        |> Seq.map(fun r -> (amp * Mathf.Cos(omega * r.x + phi) - r.y) ** 2f)
        |> Seq.average
        
    let scale =
        sqrt (amp * (rs'[0].x - rs'[rs'.Length-1].x))
    
    {LineData = Vigor(centroid, dir, amp, omega, phi); Error = err/scale}

let pointGen (fitter: Vector2 array -> Fit) (points : Vector2 array) =
    let line = fitter points
    match line.LineData with
    | WardingCircle(pos, r) ->
        0f
        |> Seq.unfold(fun theta -> if theta >= 2f * Mathf.Pi then None else Some(theta, theta+1f/r))
        |> Seq.map(fun theta -> Vector2(Mathf.Cos theta, Mathf.Sin theta) * r + pos)
        |> Array.ofSeq
    | WardingEllipse(pos, dir, a, b) ->
        0f
        |> Seq.unfold(fun theta -> if theta >= 2f * Mathf.Pi then None else Some(theta, theta+1f/a))
        |> Seq.map(fun theta -> pos +
                                   Vector2(Mathf.Cos theta, Mathf.Sin theta).Rotated(dir.Angle()) *
                                   (a * b / sqrt((b * Mathf.Cos theta) ** 2f + (a * Mathf.Sin theta) ** 2f)))
        |> Array.ofSeq
    | Forbiddance(p1, p2) ->
        let tSpace = 10f/(p1 - p2).Length()
        0f
        |> Seq.unfold(fun t -> if t > 1f then None else Some(t, t + tSpace))
        |> Seq.map(fun t -> p1.LinearInterpolate(p2, t))
        |> Array.ofSeq
    | Vigor(pos, dir, amp, omega, phi) ->
        let length = (points[0] - points[points.Length - 1]).Length()
        let tSpace = 1f
        -length
        |> Seq.unfold(fun t -> if t > length then None else Some(t, t + tSpace))
        |> Seq.map(fun t -> Vector2(t, amp * Mathf.Cos(omega * t + phi)).Rotated(dir.Angle()) + pos)
        |> Array.ofSeq
    | _ -> [||]

let getFitter(name) =
    match name with
    | 'w' -> wardingCircle
    | 'f' -> forbiddance
    | 'e' -> wardingEllipse
    | 'v' -> vigor
    | _ -> fun _ -> {LineData = Invalid; Error = infinityf}