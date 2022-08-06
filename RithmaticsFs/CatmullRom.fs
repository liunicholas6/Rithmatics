module RithmaticsFs.CatmullRom

open MathNet.Numerics.LinearAlgebra
open Godot

let segLength (points : Vector2 seq) =
    points
    |> Seq.pairwise
    |> Seq.map(fun (a, b) -> (a - b).Length())
    |> Seq.sum

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

type splineCurve() =
    let res = 10f
    let controlPoints = ResizeArray<Vector2>()
    let mutable interPoints = ResizeArray<Vector2>()
    let segLengths = ResizeArray<float32>()
    let mutable endLength = 0f
    let mutable endCap = Seq.empty
    member this.ControlPoints = controlPoints
    member this.AddPoint pt =
        controlPoints.Add(pt)
        if (controlPoints.Count > 2) then
            let ctrl =
                if controlPoints.Count = 3 then
                    [|0; 0; 1; 2|] |> Array.map(fun i -> controlPoints[i])
                else
                    [|-4 .. -1|] |> Array.map(fun i -> controlPoints[controlPoints.Count + i])
            let splinePoints = splinePts res ctrl |> Seq.cache
            segLengths.Add(segLength splinePoints)
            interPoints.AddRange(splinePoints)
            for i in 0 .. 2 do
                ctrl[i] <- ctrl[i + 1]
            endCap <- splinePts res ctrl |> Seq.cache
    member this.GetPoints() =
        if controlPoints.Count < 3 then
            controlPoints.ToArray()
        else
            seq {
                yield! interPoints
                yield! endCap
            } |> Seq.toArray
            
    member this.Spacing() =
        let length = segLengths |> Seq.sum |> (+) endLength
        seq {
            yield 0f
            yield!
                segLengths
                |> Seq.map(fun x -> x/length)
                |> Seq.scan (+) 0f
            yield 1f                
        }
        
    member this.Trim() =
        interPoints <- seq {
            let mutable curr = interPoints[0]
            yield curr
            for point in interPoints do
                if (curr - point).Length() >= res then
                    yield point
                    curr <- point
        }
        |> ResizeArray
        
    member this.Clear() =
        controlPoints.Clear()
        interPoints.Clear()
        