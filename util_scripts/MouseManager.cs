using Godot;
using System;

public class MouseManager : Node2D
{
    private bool _mouseDown;
    private bool _clickChange;

    public float MousePressure { get; private set; }
    public Vector2 MousePosition { get; private set; }
    
    public enum MouseState
    {
        Click,
        Hold,
        Release,
        Raised
    }
    
    public override void _Input(InputEvent @event)
    {
        switch (@event)
        {
            // Mouse in viewport coordinates.
            case InputEventMouseButton eventMouseButton:
            {
                if (eventMouseButton.ButtonIndex == (int)ButtonList.Left)
                {
                    _clickChange = true;
                    _mouseDown = eventMouseButton.Pressed;
                    MousePosition = eventMouseButton.Position;
                    if (!_mouseDown)
                    {
                        MousePressure = 0;
                    }
                }

                break;
            }
            case InputEventMouseMotion eventMouseMotion:
                MousePosition = eventMouseMotion.Position;
                MousePressure = eventMouseMotion.Pressure;
                break;
        }
    }

    public MouseState GetMouseState()
    {
        MouseState res = (_mouseDown, _clickChange) switch
        {
            (true, true) => MouseState.Click,
            (true, false) => MouseState.Hold,
            (false, true) => MouseState.Release,
            (false, false) => MouseState.Raised
        };
        _clickChange = false;
        return res;
    }

}