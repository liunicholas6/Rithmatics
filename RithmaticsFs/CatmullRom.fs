module RithmaticsFs.CatmullRom

open MathNet.Numerics.LinearAlgebra
open Godot

let splineFn =
    let m = matrix [[ 0f;  2f;  0f;  0f]
                    [-1f;  0f;  1f;  0f]
                    [ 2f; -5f;  4f; -1f]
                    [-1f;  3f; -3f;  1f]]

    fun (ctrlPts: Vector2 array) ->
        let coeff = 
            DenseMatrix.init 4 2 (fun i j ->
            let pt = ctrlPts[i]
            if j = 0 then pt.x else pt. y)
            |> fun ctrl -> 0.5f * m * ctrl

        fun (t : float32) ->
            let tVec =
                Seq.unfold(fun x -> Some(x, x * t)) 1f
                |> Seq.take 4
                |> Seq.map(fun x -> seq {x})
                |> DenseMatrix.ofColumnSeq

            let ansMat = tVec * coeff
            Vector2(ansMat[0, 0], ansMat[0, 1])

let splinePts (res: float32) (ctrl: Vector2 array) =
    let tSpace = res / (ctrl[2] - ctrl[1]).Length()
    Seq.unfold(fun t -> if t > 1f then None else Some(t, t + tSpace)) 0f
    |> Seq.map(splineFn ctrl)

type curveState =
        | Empty
        | Point
        | Line
        | Spline

type splineCurve() =
    let res = 10f
    let ctrl = Array.zeroCreate 4
    let mutable (interpolatedPoints : ResizeArray<Vector2>) = new ResizeArray<Vector2>()
    let mutable (endCap : Vector2 seq) = Seq.empty
    let mutable curveState = Empty

    member this.AddPoint pt =
        match curveState with
        | Empty ->
            ctrl[0] <- pt
            ctrl[1] <- pt
            curveState <- Point
        | Point ->
            ctrl[2] <- pt
            curveState <- Line
        | _ ->
            ctrl[3] <- pt
            interpolatedPoints.AddRange(splinePts res ctrl)
            for i in 0 .. 2 do
                ctrl[i] <- ctrl[i + 1]
            endCap <- splinePts res ctrl |> Seq.cache
            curveState <- Spline

    member this.GetPoints() =
        match curveState with
        | Empty -> Array.empty
        | Point -> [|ctrl[0]|]
        | Line ->  [|ctrl[1]; ctrl[2]|]
        | Spline ->
            seq {
                yield! interpolatedPoints
                yield! endCap
            } |> Seq.toArray
    member this.Clear() =
        curveState <- Empty
        interpolatedPoints.Clear()