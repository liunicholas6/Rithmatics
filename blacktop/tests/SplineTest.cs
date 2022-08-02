using Godot;
using System;
using Microsoft.FSharp.Core;
using RithmaticsFs;

public class SplineTest : Line2D
{
    private MouseManager _mouseManager;
    private readonly CatmullRom.splineCurve _curve = new();
    
    public override void _Ready()
    {
        _mouseManager = new MouseManager();
        AddChild(_mouseManager);
    }

    public override void _Process(float delta)
    {
        if (_mouseManager.GetMouseState() != MouseManager.MouseState.Click) return;
        _curve.AddPoint(_mouseManager.MousePosition);
        Points = _curve.GetPoints();
        Update();
    }

    public override void _Draw()
    {
        base._Draw();
        foreach (var point in _curve.GetPoints())
        {
            DrawCircle(point, 2, Colors.White);
        }
        foreach (var point in _curve.ControlPoints)
        {
            DrawCircle(point, 2, Colors.Black);
        }
    }
    
    public override void _Input(InputEvent @event)
    {
        if (@event is not InputEventKey inputEventKey) return;
        var key =
            inputEventKey.Scancode switch
            {
                (uint)KeyList.W => 'w',
                (uint)KeyList.F => 'f',
                (uint)KeyList.E => 'e',
                (uint)KeyList.V => 'v',
                _ => 'i'
            };
        Line2D ideal = new();
        AddChild(ideal);
        ideal.Points = Fitting.pointGen( Fitting.getFitter(key), Points);
    }
}
