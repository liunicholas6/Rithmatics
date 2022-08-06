module RithmaticsFs.Fitting.FitGet
open RithmaticsFs.Builders
open FitDef
open Godot

let display fit =
    let line = new Line2D()
    line.Position <- fit.Position
    line.Rotation <- fit.Angle
    line.Points <-
        seq {0f .. 0.05f .. 1f}
        |> Seq.map fit.Equation.Predict
        |> Seq.toArray
    line

let displayFromKey c points =
    maybe {
        let! fitter =
            match c with
            | 'w' -> Some WardingCircle.fitter
            | 'e' -> Some WardingEllipse.fitter
            | 'f' -> Some Forbiddance.fitter
            | _ -> None
        let! fit = fitter points
        let line2d = display fit
        return line2d
    } |> Option.fold(fun _ v -> v) (new Line2D())