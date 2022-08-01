using Godot;
using System;
using System.Collections.Generic;
using RithmaticsFs;

public class InputManager : Node2D
{
    private MouseManager _mouseManger;
    private Line2D _line;
    private List<float> _weights = new();
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
                break;
            case MouseManager.MouseState.Hold:
                point = _mouseManger.MousePosition;
                _curve.AddPoint(point);
                _line.Points = _curve.GetPoints();
                _weights.Add(_mouseManger.MousePressure);
                Curve widthCurve = new Curve();
                for (int i = 0; i < _weights.Count; i++)
                {
                    widthCurve.AddPoint(new Vector2((float) i / _weights.Count, _weights[i]));
                }
                _line.WidthCurve = widthCurve;
                break;
            case MouseManager.MouseState.Release:
                point = _mouseManger.MousePosition;
                _curve.AddPoint(point);
                _curve.AddPoint(point);
                _line.Points = _curve.GetPoints();
                break;
        }
    }
}
