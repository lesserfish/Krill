#nullable enable
using System;
using System.Collections.Generic;

namespace Nez.Extension;

public class ClipBundle : IClip {
    public ClipBundle(IClip requiredClip, params IClip[] optionalClips){
        _clipBundle = new List<IClip>();
        _clipBundle.Add(requiredClip);
        foreach(IClip clip in optionalClips){
            _clipBundle.Add(clip);
        }

        foreach(IClip clip in _clipBundle){
            clip.SetLoop(0);
            clip.SetOnEnd(null);
            clip.SetOnLoop(null);
            _endTime = Math.Max(_endTime, clip.GetDuration());
        }
    }
    public ClipBundle(int loop, IClip requiredClip, params IClip[] optionalClips){
        _clipBundle = new List<IClip>();
        _clipBundle.Add(requiredClip);
        foreach(IClip clip in optionalClips){
            _clipBundle.Add(clip);
        }

        foreach(IClip clip in _clipBundle){
            clip.SetLoop(0);
            clip.SetOnEnd(null);
            clip.SetOnLoop(null);
            _endTime = Math.Max(_endTime, clip.GetDuration());
        }
    }

    public IClip Update(float deltaTime){
        if(Status == ClipStatus.Playing){
            _time += deltaTime;

            if(_time > _endTime){
                _handleEnd();
            } else {
                foreach(IClip clip in _clipBundle){
                    clip.Update(deltaTime);
                }
            }
        }
        return this;
    }
    public IClip Start(){
        // Start all clips
        foreach(IClip clip in _clipBundle){
            clip.Start();
        }

        Status = ClipStatus.Playing;
        return this;
    }
    public IClip Pause(){
        // Pause all clips
        foreach(IClip clip in _clipBundle){
            clip.Pause();
        }
        Status = ClipStatus.Paused;
        return this;
    }
    public IClip Reset(){
        // Reset all clips
        foreach(IClip clip in _clipBundle){
            clip.Reset();
            clip.SetLoop(0);
        }

        Status = ClipStatus.Stopped;
        return this;
    }
    public IClip Jump(float time){
        _time = time;
        foreach(IClip clip in _clipBundle){
            clip.Jump(time);
        }

        if(time >= _endTime){
            _handleEnd();
        }
        return this;
    }
    public IClip SetLoop(int loop){
        // Set All clips to loop
        Loop = _loop;
        return this;
    }
    public IClip SetOnEnd(Action<IClip> action){
        OnEnd = action;
        return this;
    }
    public IClip SetOnLoop(Action<IClip> action){
        OnLoop = action;
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
    ClipStatus Status;
    int _loop;
    float _time;
    float _endTime;
    List<IClip> _clipBundle;
}
