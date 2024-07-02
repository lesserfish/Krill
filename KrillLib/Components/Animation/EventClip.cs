#nullable enable
using System;
using System.Collections.Generic;
using System.Reflection;

namespace Nez.Extension;

public interface IEvent {
    void Trigger();
}

public class SimpleEvent : IEvent {
    public SimpleEvent(Action trigger){
        _trigger = trigger;
    }
    public void Trigger(){
        _trigger();
    }
    Action _trigger;
}

public class MethodEvent: IEvent {
    public MethodEvent(object target, string method, params object[] parameters){
        var methodInfo = target.GetType().GetMethod(method);
        if(methodInfo == null){
            throw new ArgumentException("Could not find method in target object");
        }

        _target = target;
        _parameters = parameters;
        _methodInfo = methodInfo;
    }

    public void Trigger(){
        _methodInfo.Invoke(_target, _parameters);
    }
    private object _target;
    private object[] _parameters;
    private MethodInfo _methodInfo;
}

public class EventKeyframe {
    public EventKeyframe(float time, IEvent ev){
        Time = time;
        Event = ev;
    }
    public float Time;
    public IEvent Event;
}


public class EventClip : IClip {
    public EventClip(List<EventKeyframe> keyframes){
        _keyframes = keyframes;
        _endTime = keyframes.LastItem().Time;
    }

    public IClip Update(float deltaTime){
        if(Status == ClipStatus.Playing){
            _time += deltaTime;

            if(_time > _endTime){
                // Execute the remaining keyframes
                for(; _currentKeyframe < _keyframes.Count; _currentKeyframe++){
                    _keyframes[_currentKeyframe].Event.Trigger();
                }
                _handleEnd();
                return this;
            }
            
            while(_time > _keyframes[_currentKeyframe].Time){
                _keyframes[_currentKeyframe].Event.Trigger();
                _currentKeyframe++;
            }
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
    public IClip Stop(){
        Status = ClipStatus.Stopped;
        _time = 0;
        return this;
    }
    public IClip Jump(float time){
        _time = time;
        if(time >= _endTime){
            _handleEnd();
        }
        _calculateCurrentKeyframe();
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
            _currentKeyframe = 0;
            Status = ClipStatus.Playing;
            if(OnLoop is not null){
                OnLoop(this);
            }
        } else {
            _time = 0;
            _currentKeyframe = 0;
            Status = ClipStatus.Stopped;
            if(OnEnd is not null){
                OnEnd(this);
            }
        }
    }
    private void _calculateCurrentKeyframe(){
        for(int i = 0; i < _keyframes.Count; i++){
            var keyframe= _keyframes[i];
            if(keyframe.Time >= _time){
                _currentKeyframe = i;
                return;
            }
        }
        _currentKeyframe = _keyframes.Count;
        _handleEnd();
    }

    public Action<IClip>? OnEnd;
    public Action<IClip>? OnLoop;
    public int Loop;
    public ClipStatus Status {get; private set;}

    int _currentKeyframe;
    float _time;
    float _endTime;
    List<EventKeyframe> _keyframes; 
}
