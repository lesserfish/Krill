#nullable enable
global using EaseType = Nez.Tweens.EaseType;
using Microsoft.Xna.Framework;
using Nez.Tweens;


namespace Nez.Extension;

public interface ILerp<T> {
    T Lerp(EaseType? type, T start, T end, float t);
}


public class IntLerp : ILerp<int> {
    public int Lerp(EaseType? type, int start, int end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return (int)Lerps.Ease(type.Value, start, end, t, 1);
    }
}

public class FloatLerp : ILerp<float> {
    public float Lerp(EaseType? type, float start, float end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return Lerps.Ease(type.Value, start, end, t, 1);
    }
}

public class Vector2Lerp : ILerp<Vector2> {
    public Vector2 Lerp(EaseType? type, Vector2 start, Vector2 end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return Lerps.Ease(type.Value, start, end, t, 1);
    }
}

public class Vector3Lerp : ILerp<Vector3> {
    public Vector3 Lerp(EaseType? type, Vector3 start, Vector3 end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return Lerps.Ease(type.Value, start, end, t, 1);
    }

}

public class Vector4Lerp : ILerp<Vector4> {
    public Vector4 Lerp(EaseType? type, Vector4 start, Vector4 end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return Lerps.Ease(type.Value, start, end, t, 1);
    }
}

public class ColorLerp : ILerp<Color> {
    public Color Lerp(EaseType? type, Color start, Color end, float t){
        if(type is null){
            return t < 1 ? start : end;
        }
        return Lerps.Ease(type.Value, start, end, t, 1);
    }
}

public class Lerp {
    public static readonly IntLerp Int = new IntLerp();
    public static readonly FloatLerp Float = new FloatLerp();
    public static readonly Vector2Lerp V2 = new Vector2Lerp();
    public static readonly Vector3Lerp V3 = new Vector3Lerp();
    public static readonly Vector4Lerp V4 = new Vector4Lerp();
    public static readonly ColorLerp Color = new ColorLerp();
}
