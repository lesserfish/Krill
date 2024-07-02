#nullable enable
using System;
using System.Collections.Generic;

namespace Nez.Extension;

public struct Keyframe<T> where T : struct {
    public float time;
    public T value;
}

public struct AnimationSegment<T> where T : struct {
    public Keyframe<T> Start;
    public Keyframe<T> End;
    public EaseType? Ease;
}


public class AnimationClip<T> : IClip where T : struct {
    public AnimationClip(IAnimationTarget<T> target, List<AnimationSegment<T>> segments, int loop, ILerp<T> lerper){
        _target = target;
        _segments = segments;
        _loop = loop;
        _lerper = lerper;

        Loop = loop;
        _time = 0;
        _currentSegment = 0;
        _endTime = segments.LastItem().End.time;
    }

    public IClip Update(float deltaTime){
        if(Status == ClipStatus.Playing){
            _time += deltaTime;

            if(_time > _endTime){
                T lastValue = _lerper.Lerp(_segments.LastItem().Ease, _segments.LastItem().Start.value, _segments.LastItem().End.value, 1);
                _target.SetValue(lastValue);
                _handleEnd();
                return this;
            }
            
            while(_time > _segments[_currentSegment].End.time){
                _currentSegment++;
            }

            var segment = _segments[_currentSegment];
            float t = (_time - segment.Start.time)/(segment.End.time - segment.Start.time);
            T value = _lerper.Lerp(segment.Ease, segment.Start.value, segment.End.value, t);
            _target.SetValue(value);
        }
        return this;
    }
    public IClip Start(){
        Status = ClipStatus.Playing;
        return this;
    }
    public IClip Pause(){
        Status = ClipStatus.Paused;
        return this;
    }
    public IClip Reset(){
        Status = ClipStatus.Stopped;
        _time = 0;
        Loop = _loop;
        return this;
    }
    public IClip Jump(float time){
        _time = time;
        if(time >= _endTime){
            _handleEnd();
        }
        _calculateCurrentSegment();
        return this;
    }
    public IClip SetLoop(int loop){
        // Todo: deal with loop == 0 case. Perhaps allow it? Idk.
        Loop = loop;
        return this;
    }
    public IClip SetOnEnd(Action<IClip> onEnd){
        OnEnd = onEnd;
        return this;
    }
    public IClip SetOnLoop(Action<IClip> onLoop){
        OnLoop = onLoop;
        return this;
    }
    public int GetLoop(){
        return Loop;
    }
    public float Duration(){
        return _endTime;
    }
    public ClipStatus GetStatus(){
        return Status;
    }
    public float GetDuration(){
        return _endTime;
    }


    private void _handleEnd(){
        if(Loop > 0 || Loop < 0){
            // Decrease Loop
            Loop = Math.Max(Loop - 1, -1);
            _time = 0;
            _currentSegment = 0;
            Status = ClipStatus.Playing;
            if(OnLoop is not null){
                OnLoop(this);
            }
        } else {
            _time = 0;
            _currentSegment = 0;
            Status = ClipStatus.Stopped;
            if(OnEnd is not null){
                OnEnd(this);
            }
        }
    }
    private void _calculateCurrentSegment(){
        for(int i = 0; i < _segments.Count; i++){
            var segment = _segments[i];
            if(segment.Start.time <= _time && _time <= segment.End.time){
                _currentSegment = i;
                return;
            }
        }
        _currentSegment = _segments.Count;
        _handleEnd();
    }

    public Action<IClip>? OnEnd;
    public Action<IClip>? OnLoop;
    public int Loop;
    public ClipStatus Status {get; private set;}

    int _loop;
    int _currentSegment;
    float _time;
    float _endTime;
    List<AnimationSegment<T>> _segments;
    IAnimationTarget<T> _target;
    ILerp<T> _lerper;
}

