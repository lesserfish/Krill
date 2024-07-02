using System;

namespace Nez.Extension;

public enum ClipStatus {
    Paused,
    Playing,
    Stopped
}

public interface IClip {
    IClip Update(float deltaTime);
    IClip Start();
    IClip Pause();
    IClip Reset();
    IClip Jump(float time);
    IClip SetLoop(int loop);
    IClip SetOnEnd(Action<IClip> action);
    IClip SetOnLoop(Action<IClip> action);
    int GetLoop();
    float Duration();
    ClipStatus GetStatus();
    float GetDuration();
}



