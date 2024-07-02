#nullable enable
using System;
using System.Collections.Generic;

namespace Nez.Extension;

public struct EventKeyframe {
    float time;
    Action action;
}

public class EventClip : IClip {
    public EventClip(List<EventKeyframe> keyframes, int loop){
        _keyframes = keyframes;
        _loop = loop;
        Loop = loop;
        _time = 0;
    }
    public IClip Update(float deltaTime){
        // Todo
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
        return this;
    }
    public IClip SetLoop(int loop){
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
        return 0;
    }


    private void _handleEnd(){
        if(Loop > 0 || Loop < 0){
            // Decrease Loop
            Loop = Math.Max(Loop - 1, -1);
            _time = 0;
            Status = ClipStatus.Playing;
            if(OnLoop is not null){
                OnLoop(this);
            }
        } else {
            _time = 0;
            Status = ClipStatus.Stopped;
            if(OnEnd is not null){
                OnEnd(this);
            }
        }
    }

    public Action<IClip>? OnEnd;
    public Action<IClip>? OnLoop;
    public int Loop;
    public ClipStatus Status {get; private set;}


    int _loop;
    float _time;
    float _endTime;
    List<EventKeyframe> _keyframes;
}
