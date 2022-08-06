module RithmaticsFs.Fitting.WardingEllipse
open FitDef
open Godot

type Equation(a : float32, b: float32) =
    interface IRithmaticEquation with
        member this.ErrorNorm = pown a 2 + pown b 2 |> sqrt
        member this.Predict t =
            let theta = Mathf.Tau * t
            let v = Vector2(Mathf.Cos theta, Mathf.Sin theta)
            v * this.DirectionLength v
        member this.Residual v =
            v.Length() - (this.DirectionLength v)
    member this.DirectionLength (v : Vector2) =
        0.5f * a * b / sqrt((b * v.x) ** 2f + (a * v.y) ** 2f)
let fitter  =
    let range sequence =
        sequence
        |> Seq.fold(fun (mn, mx) elem -> (min mn elem, max mx elem)) (infinityf, -infinityf)
        |> fun (mn, mx) -> mx - mn
    fun (points : Vector2 array) ->
        let centroid, angle, rs = deming points
        let a = rs |> Seq.map(fun v -> v.x) |> range
        let b = rs |> Seq.map(fun v -> v.y) |> range
        Some {Position = centroid; Angle = angle; Points = rs; Equation = Equation(a, b)}