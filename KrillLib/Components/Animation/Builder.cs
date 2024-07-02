#nullable enable
using System;
using Microsoft.Xna.Framework;
using System.Collections.Generic;

namespace Nez.Extension;

public class ClipBuilder<T> where T : struct {
    private ClipBuilder(IAnimationTarget<T> target, T initialValue, ILerp<T> lerper){
        _segments = new List<AnimationSegment<T>>();
        _target = target;
        _keytime = 0;
        _lastValue = initialValue;
        _lerper = lerper;
        _loop = 0; // A single loop
    }
    public static ClipBuilder<T> Start(IAnimationTarget<T> target, ILerp<T> lerper, T initialValue){
        return new ClipBuilder<T>(target, initialValue, lerper);
    }
    public static ClipBuilder<int> Start(IAnimationTarget<int> target, int initialValue){
        return new ClipBuilder<int>(target, initialValue, Lerp.Int);
    }
    public static ClipBuilder<float> Start(IAnimationTarget<float> target, float initialValue){
        return new ClipBuilder<float>(target, initialValue, Lerp.Float);
    }
    public static ClipBuilder<Vector2> Start(IAnimationTarget<Vector2> target, Vector2 initialValue){
        return new ClipBuilder<Vector2>(target, initialValue, Lerp.V2);
    }
    public static ClipBuilder<Vector3> Start(IAnimationTarget<Vector3> target, Vector3 initialValue){
        return new ClipBuilder<Vector3>(target, initialValue, Lerp.V3);
    }
    public static ClipBuilder<Vector4> Start(IAnimationTarget<Vector4> target, Vector4 initialValue){
        return new ClipBuilder<Vector4>(target, initialValue, Lerp.V4);
    }
    public static ClipBuilder<Color> Start(IAnimationTarget<Color> target, Color initialValue){
        return new ClipBuilder<Color>(target, initialValue, Lerp.Color);
    }
    public static ClipBuilder<Quaternion> Start(IAnimationTarget<Quaternion> target, Quaternion initialValue){
        return new ClipBuilder<Quaternion>(target, initialValue, Lerp.Quaternion);
    }
    public static ClipBuilder<Rectangle> Start(IAnimationTarget<Rectangle> target, Rectangle initialValue){
        return new ClipBuilder<Rectangle>(target, initialValue, Lerp.Rectangle);
    }

    public ClipBuilder<T> AddKey(float time, T value, EaseType? ease = null){
        if(time <= _keytime){
            throw new ArgumentException("Unordered keyframes.");
        }
        var startKeyframe = new Keyframe<T> {time = _keytime, value = _lastValue};
        var endKeyframe = new Keyframe<T> {time = time, value = value};
        var segment = new AnimationSegment<T>{Start = startKeyframe, End = endKeyframe, Ease = ease};

        _segments.Add(segment);

        _keytime = time;
        _lastValue = value;

        return this;
    }

    public ClipBuilder<T> Extend(float time){
        if(time <= _keytime){
            throw new ArgumentException("Unordered keyframes.");
        }
        var startKeyframe = new Keyframe<T> {time = _keytime, value = _lastValue};
        var endKeyframe = new Keyframe<T> {time = time, value = _lastValue};
        var segment = new AnimationSegment<T>{Start = startKeyframe, End = endKeyframe, Ease = null};

        _segments.Add(segment);

        _keytime = time;

        return this;
    }
    
    public ClipBuilder<T> Loop(int loop = -1){
        _loop = loop;
        return this;
    }
    public ClipBuilder<T> OnEnd(Action<IClip> onEnd){
        _onEnd = onEnd;
        return this;
    }
    public ClipBuilder<T> OnLoop(Action<IClip> onLoop){
        _onLoop = onLoop;
        return this;
    }
    public AnimationClip<T> Finish(){
        AnimationClip<T> animation = new AnimationClip<T>(_target, _segments, _loop, _lerper);
        animation.OnEnd = _onEnd;
        animation.OnLoop = _onLoop;
        return animation; 
    }
    
    int _loop;
    T _lastValue;
    float _keytime;
    IAnimationTarget<T> _target;
    List<AnimationSegment<T>> _segments;
    ILerp<T> _lerper;
    Action<IClip>? _onEnd;
    Action<IClip>? _onLoop;

}


public class EventClipBuilder {
    // Todo
}


