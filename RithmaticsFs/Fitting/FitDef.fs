module RithmaticsFs.Fitting.FitDef
open System.Numerics
open Godot
type IRithmaticEquation =
    abstract member ErrorNorm : float32
    abstract member Predict : float32 -> Vector2
    abstract member Residual : Vector2 -> float32

type Fit = {
    Position: Vector2
    Angle: float32
    Points: Vector2 array
    Equation: IRithmaticEquation
}
type Fitter = Vector2 array -> Fit option
let mse fit =
    fit.Points
    |> Seq.map (fun pt -> fit.Equation.Residual pt |> fun x -> pown x 2)
    |> Seq.average
    |> fun x -> x / fit.Equation.ErrorNorm
    
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
    let angle = float32(z.Phase)
    let rs' = rs |> Array.map(fun x -> x.Rotated(-angle))
    centroid, angle, rs'
    
