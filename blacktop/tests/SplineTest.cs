using Godot;
using System;
using RithmaticsFs;

public class SplineTest : Node2D
{
    private MouseManager _mouseManager;
    private readonly CatmullRom.splineCurve _curve = new CatmullRom.splineCurve();
    public override void _Ready()
    {
        _mouseManager = new MouseManager();
        AddChild(_mouseManager);
    }

    public override void _Process(float delta)
    {
        if (_mouseManager.GetMouseState() == MouseManager.MouseState.Click)
        {
            _curve.AddPoint(_mouseManager.MousePosition);
            Update();
        }
    }

    public override void _Draw()
    {
        foreach (Vector2 point in _curve.GetPoints())
        {
            DrawCircle(point, 2, Colors.White);
        }
    }
}
