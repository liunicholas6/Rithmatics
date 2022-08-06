module RithmaticsFs.Fitting.Forbiddance
open FitDef
open Godot

type Equation(l : float32) =
    interface IRithmaticEquation with
        member this.ErrorNorm = l
        member this.Predict t =
            Vector2.Right * t * l
        member this.Residual v =
            v.y
let fitter (points : Vector2 array) =
    let centroid, angle, rs = deming points
    let length = rs[0].x - rs[rs.Length-1].x
    let position = centroid - (rs[0].x * Vector2(Mathf.Cos angle, Mathf.Sin angle))
    Some {Position = position; Angle = angle; Points = rs; Equation = Equation(length)}   