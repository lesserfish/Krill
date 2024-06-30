#nullable enable
using System;

namespace Nez.Extension;

public abstract class FlowController {
    public virtual void Action(ControllableEntity? entity, Action next){
        next();
    }
    public FlowController Join(FlowController next){
        return new FlowChain(this, next);
    }
    public static FlowController New(){
        return new GenericFlow();
    }
}

public class GenericFlow : FlowController {
    public override void Action(ControllableEntity? entity, Action next){
        next();
    }
}
public class FlowChain : FlowController{
    public FlowChain(FlowController a, FlowController b){
        _a = a;
        _b = b;
    }

    public override void Action(ControllableEntity? entity, Action next){
        _a.Action(entity, () => {_b.Action(entity, next);});
    }

    private FlowController _a;
    private FlowController _b;
}

public class ControllableEntity : Nez.Entity {
    public ControllableEntity(FlowController controller) : base() {
        _controller = controller;
    }
    public ControllableEntity(string name, FlowController controller) : base(name) {
        _controller = controller;
    }

    public override void Update(){
        Action next = () => base.Update();
        _controller.Action(this, next);
    }
    protected FlowController _controller;
}

public class PauseController : FlowController {
    public PauseController(bool paused = false){
        Paused = paused;
    }
    public override void Action(ControllableEntity? context, Action next){
        if(!Paused){
            next();
        }
    }
    public bool Paused;
}

public class PausableEntity : ControllableEntity {
    public PausableEntity(string name, bool paused = false) : base(name, new PauseController(paused)){}
    public PausableEntity(bool paused = false) : base(new PauseController(paused)){}

    public bool Paused {
        get {
            var p = _controller as PauseController;
            if(p is null){
                throw new NullReferenceException("For some reason, controller is null or not a PausableController");
            }
            return p.Paused;

        }
        set {
            var p = _controller as PauseController;
            if(p is null){
                throw new NullReferenceException("For some reason, controller is null or not a PausableController");
            }
            p.Paused = value;
        }
    }
}

