#nullable enable

namespace Nez.Extension;

// A very simple Clip Player that allows you to play clips without needing to set up an entire Animator
public class ClipPlayer : Nez.Component, IUpdatable {
    public ClipPlayer() : base(){}
    
    public ClipPlayer SetClip(IClip clip){
        Clip = clip;
        return this;
    }
    public void Update(){
        float deltaTime = Nez.Time.DeltaTime;
        if(Clip is not null){
            Clip.Update(deltaTime);
        }
    }
    public override void OnRemovedFromEntity(){
        Clip = null;
    }
    public IClip? Clip;
}
