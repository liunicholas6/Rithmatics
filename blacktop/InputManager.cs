using Godot;
using System;
using RithmaticsFs;

public class InputManager : Node2D
{
    private MouseManager _mouseManger;
    private Line2D _line;
    private Vector2 _prevPt;
    private const float Res = 5;
    private readonly CatmullRom.splineCurve _curve = new();
    
    public override void _Ready()
    {
        _mouseManger = GetNode<MouseManager>("MouseManager");
        _line = GetNode<Line2D>("Line");
    }

    public override void _Process(float delta)
    {
        Vector2 point;
        switch (_mouseManger.GetMouseState())
        {
            case MouseManager.MouseState.Click:
                point = _mouseManger.MousePosition;
                _curve.AddPoint(point);
                _line.AddPoint(point);
                _prevPt = point;
                break;
            case MouseManager.MouseState.Hold:
                point = _mouseManger.MousePosition;
                if ((point - _prevPt).Length() > Res)
                {
                    GD.Print("Added");
                    _curve.AddPoint(point);
                    _prevPt = point;
                    _line.Points = _curve.GetPoints();
                }
                break;
            case MouseManager.MouseState.Release:
                GD.Print("release");
                point = _mouseManger.MousePosition;
                _curve.AddPoint(point);
                _curve.AddPoint(point);
                _line.Points = _curve.GetPoints();
                break;
        }

        Update();
    }

    public override void _Draw()
    {
        foreach (Vector2 pt in _curve.GetPoints())
        {
            DrawCircle(pt, 2, Colors.Green);
        }
    }
}
