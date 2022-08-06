module RithmaticsFs.Fitting.WardingCircle
open FitDef
open Godot

type Equation(r : float32) =
    interface IRithmaticEquation with
        member this.ErrorNorm = r
        member this.Predict t =
            let theta = Mathf.Tau * t
            r * Vector2(Mathf.Cos theta, Mathf.Sin theta)
        member this.Residual v =
            v.Length() - r
let fitter (points : Vector2 array) =
    let centroid = Array.sum points / float32(points.Length)
    let rs = points |> Array.map(fun pt -> pt - centroid)
    let r = rs |> Seq.map(fun r -> r.Length()) |> Seq.average
    Some {Position = centroid; Angle = 0f; Points = rs; Equation = Equation(r)}