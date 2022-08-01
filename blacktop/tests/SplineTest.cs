using Godot;
using System;
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
        
    }

    public override void _Draw()
    {
        base._Draw();
        foreach (var point in _curve.GetPoints())
        {
            DrawCircle(point, 2, Colors.White);
        }
    }
}
