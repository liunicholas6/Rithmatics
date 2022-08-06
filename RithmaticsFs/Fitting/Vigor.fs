module RithmaticsFs.Fitting.Vigor
open RithmaticsFs.Builders
open Godot

let smooth (points : Vector2 seq) =
    points
    |> Seq.windowed 5
    |> Seq.map(fun window -> Seq.sum window / float32(window.Length))
let curvature (window : Vector2 array) =
    (window[1] - window[0]).Normalized().Cross((window[2] - window[1]).Normalized())

type Sign =
    | Negative
    | Zero
    | Positive
let categorize =
    let curvatureSign x =
        let c = curvature x
        if c < -0.15f then Negative
        elif c > 0.15f then Positive
        else Zero
    let findCenter (arr : ResizeArray<Vector2>) =
        if arr.Count = 0 then None
        elif arr.Count % 2 = 0 then Some(arr[arr.Count / 2 - 1] + arr[arr.Count / 2] |> (*) 0.5f)
        else Some arr[arr.Count / 2]
    fun (points : Vector2 array) ->
        let buffer = ResizeArray<Vector2>()
        let map = [(Negative, ResizeArray<Vector2>())
                   (Zero, ResizeArray<Vector2>())
                   (Positive, ResizeArray<Vector2>())] |> Map
        let mutable cat = curvatureSign points[0..2]
        for window in points |> Seq.windowed 3 do
            let sign = curvatureSign window
            if sign <> cat then do
                findCenter buffer |> Option.fold(fun _ -> map[cat].Add) ()
                buffer.Clear()
                cat <- sign
            buffer.Add window[1]
        map