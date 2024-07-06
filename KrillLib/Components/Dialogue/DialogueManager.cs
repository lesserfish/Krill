#nullable enable

using System;
using System.Collections.Generic;
using MoonSharp.Interpreter;

namespace Nez.Extension;

public class DialogueException : Exception {
    public DialogueException(string message, int errorCode) : base(message){
        ErrorCode = errorCode;
    }
    public int ErrorCode;
}

public enum DialogueState {
    RenderingText,
    WaitingOk,
    WaitingReply,
    Sleeping,
    Stopped,
    RunningLua
};

public class DialogueManager : Nez.Component, IUpdatable {

    static class Triggers {
        public static string GOTO_RENDERING_TEXT = "GOTO_RENDERING_TEXT";
        public static string GOTO_STOPPED = "GOTO_STOPPED";
        public static string GOTO_SLEEP = "GOTO_SLEEP";
        public static string GOTO_WAITING_OK = "GOTO_WAITING_OK";
        public static string GOTO_WAITING_REPLY = "GOTO_WAITING_REPLY";
        public static string RECEIVED_OK = "RECEIVED_OK";
        public static string RECEIVED_REPLY = "RECEIVED_REPLY";
        public static string RENDER_COMPLETE = "RENDER_COMPLETE";
        public static string RENDER_COMPLETE_OK = "RENDER_COMPLETE_OK";
        public static string SLEEP_COMPLETE = "SLEEP_COMPLETE";
    }

    enum InteractionMessage {
        Clear=0,
        Say,
        Pause,
        Sleep,
        Ask,
        Goto,
        Custom,
        Stop
    }
    class RenderingText : SimpleState<DialogueState> {
        public RenderingText(DialogueManager manager) : base (DialogueState.RenderingText) {
            _manager = manager;
        }
        public override void OnStart(StateMachine owner){
            _manager._timeBuffer = 0;
        }
        public override void Update(StateMachine owner){
            string text = _manager._text;
            long visibility = _manager._visibility;

            _manager._timeBuffer += Nez.Time.DeltaTime;

            if(_manager._timeBuffer > (1.0f/_manager.TextSpeed)){
                
                // Increase one character of visible text
                visibility = System.Math.Min(visibility + 1, text.Length);

                // Skip all characters following a '\' character until a space or a EOL is found
                if(visibility < text.Length && _manager.SkipSpecial){
                    if(text[(int)visibility] == '\\'){

                        while(visibility < text.Length){
                            if(text[(int) visibility++] == ' '){
                                break;
                            }
                        }
                    }
                }

                if(visibility == text.Length){
                    _manager._visibility = visibility;
                    if(_manager.PauseAfterSay){
                        owner.Trigger(Triggers.RENDER_COMPLETE_OK);
                    } else {
                        owner.Trigger(Triggers.RENDER_COMPLETE);
                    }
                }
                _manager._timeBuffer = 0;
            }
            _manager._visibility = visibility;
        }
        private DialogueManager _manager;
    }
    class Sleeping : SimpleState<DialogueState> {
        public Sleeping() : base(DialogueState.Sleeping){}
        public override void Update(StateMachine owner){
            object? oSleepTime = owner.GetParameter("sleepTime");
            if(oSleepTime is not float){
                owner.Trigger(Triggers.SLEEP_COMPLETE);
                return;
            }

            float sleepTime = (float) oSleepTime;
            sleepTime -= Nez.Time.DeltaTime;

            if(sleepTime < 0){
                owner.Trigger(Triggers.SLEEP_COMPLETE);
            }

            owner.UpdateParameter("sleepTime", sleepTime);
        }
    }

    class RunningLua : SimpleState<DialogueState> {
        public RunningLua(DialogueManager manager) : base(DialogueState.RunningLua){
            _manager = manager;
        }
        public override void Update(StateMachine owner){
            if(_manager._coroutine.Type != DataType.Thread){
                _manager._coroutine = DynValue.Nil;
                owner.Trigger(Triggers.GOTO_STOPPED);
                return;
            }

            DynValue output = _manager._coroutine.Coroutine.Resume();

            InteractionMessage message;
            DynValue arg = DynValue.Nil;


            if(output.Type == DataType.Number){
                message = (InteractionMessage) ((int) output.Number);
            }
            else if(output.Type == DataType.Tuple){
                if(output.Tuple[0].Type != DataType.Number){
                    throw new DialogueException("Received unknown interaction message from Lua.", 1);
                }
                message = (InteractionMessage) ((int) output.Tuple[0].Number);
                arg = output.Tuple[1];
            }
            else if(output.Type == DataType.Nil){
                if(_manager.AllowEmptyMessages){
                    owner.Trigger(Triggers.GOTO_STOPPED);
                    return;
                } else {
                    throw new DialogueException("Received unknown interaction message from Lua.", 2);
                }
            }
            else {
                throw new DialogueException("Received unknown interaction message from Lua.", 3);
            }

            switch(message){
                case InteractionMessage.Clear:
                    _manager._visibility = 0;
                    _manager._text = "";
                    break;
                case InteractionMessage.Say:
                    if (arg.Type != DataType.String) {
                        throw new DialogueException("Expected message from Lua.", 4);
                    }
                    _manager._text += arg.String;
                    owner.Trigger(Triggers.GOTO_RENDERING_TEXT);
                    break;
                case InteractionMessage.Pause:
                    owner.Trigger(Triggers.GOTO_WAITING_OK);
                    break;
                case InteractionMessage.Sleep:
                    if(arg.Type != DataType.Number){
                        throw new DialogueException("Expected number from Lua.", 5);
                    }
                    owner.UpdateParameter("sleepTime", (float) arg.Number);
                    owner.Trigger(Triggers.GOTO_SLEEP);
                    break;
                case InteractionMessage.Ask:
                    if(arg.Type != DataType.Table){
                        throw new DialogueException("Expected table from Lua.", 6);
                    }
                    _manager.Options.Clear();
                    foreach(var element in arg.Table.Pairs){
                        var key = element.Key;
                        var value = element.Value;

                        if(key is null || value is null){
                            throw new DialogueException("Table contains null values", 7);
                        }
                        if(key.Type != DataType.Number){
                            throw new DialogueException("Expected integer in option key", 8);
                        }
                        if(value.Type != DataType.String){
                            throw new DialogueException("Expected string in option value", 9);
                        }
                        
                        _manager.Options.Add((int) key.Number, value.String);
                    }
                    owner.Trigger(Triggers.GOTO_WAITING_REPLY);
                    break;
                case InteractionMessage.Goto:
                    if(arg.Type != DataType.Number){
                        throw new DialogueException("Expected number from Lua.", 10);
                    }
                    int code = (int) arg.Number;
                    if(code == 0){
                        throw new DialogueException("Jumping to code 0 is not allowed.", 11);
                    }
                    DynValue function = _manager._lua.DoString(_manager._ldg.Entries[code]);
                    _manager._coroutine = _manager._lua.CreateCoroutine(function);
                    break;
                case InteractionMessage.Custom:
                    if(arg.Type != DataType.Table){
                        throw new DialogueException("Expected table from Lua.", 12);
                    }
                    _manager.Custom(arg);
                    break;
                case InteractionMessage.Stop:
                    owner.Trigger(Triggers.GOTO_STOPPED);
                    break;
            }


        }
        private DialogueManager _manager;
    }

    public DialogueManager(string ldgSource, string initScript = ""){
        _sm = new StateMachine();
        _lua = new Script();
        _coroutine = DynValue.Nil;
        _ldg = LDG.FromString(ldgSource);
        _text = "";
        Options = new Dictionary<int, string>();
        Text = "";

        _lua.DoString(initScript);


        // Set up State Machine
        //
        // States
        _sm.PushState(new SimpleState<DialogueState>(DialogueState.WaitingOk));
        _sm.PushState(new SimpleState<DialogueState>(DialogueState.WaitingReply));
        _sm.PushState(new SimpleState<DialogueState>(DialogueState.Stopped));
        _sm.PushState(new Sleeping());
        _sm.PushState(new RenderingText(this));
        _sm.PushState(new RunningLua(this), true);

        // Transitions

        // RunninggLua -> RenderingText
        _sm.PushTransitionRule(new TransitionRule(
                            DialogueState.RunningLua.ToString(), 
                            DialogueState.RenderingText.ToString(),
                            new SimpleTrigger(Triggers.GOTO_RENDERING_TEXT)));

        // RunninggLua -> Stopped
        _sm.PushTransitionRule(new TransitionRule(
                            DialogueState.RunningLua.ToString(), 
                            DialogueState.Stopped.ToString(),
                            new SimpleTrigger(Triggers.GOTO_STOPPED)));

        // RunningLua -> Sleep
        _sm.PushTransitionRule(new TransitionRule(
                            DialogueState.RunningLua.ToString(), 
                            DialogueState.Sleeping.ToString(),
                            new SimpleTrigger(Triggers.GOTO_SLEEP)));

        // RunninggLua -> WaitingOk
        _sm.PushTransitionRule(new TransitionRule(
                            DialogueState.RunningLua.ToString(), 
                            DialogueState.WaitingOk.ToString(),
                            new SimpleTrigger(Triggers.GOTO_WAITING_OK)));

        // RunninggLua -> WaitingReply
        _sm.PushTransitionRule(new TransitionRule(
                            DialogueState.RunningLua.ToString(), 
                            DialogueState.WaitingReply.ToString(),
                            new SimpleTrigger(Triggers.GOTO_WAITING_REPLY)));
        
        // RenderingText -> RunningLua
        _sm.PushTransitionRule(new TransitionRule(
                                DialogueState.RenderingText.ToString(),
                                DialogueState.RunningLua.ToString(),
                                new SimpleTrigger(Triggers.RENDER_COMPLETE)));

        // RenderingText -> WaitingOk
        _sm.PushTransitionRule(new TransitionRule(
                                DialogueState.RenderingText.ToString(),
                                DialogueState.WaitingOk.ToString(),
                                new SimpleTrigger(Triggers.RENDER_COMPLETE_OK)));

        // WaitingOK -> RunningLua
        _sm.PushTransitionRule(new TransitionRule(
                                DialogueState.WaitingOk.ToString(),
                                DialogueState.RunningLua.ToString(),
                                new SimpleTrigger(Triggers.RECEIVED_OK)));

        // WaitingReply -> RunningLua
        _sm.PushTransitionRule(new TransitionRule(
                                DialogueState.WaitingReply.ToString(),
                                DialogueState.RunningLua.ToString(),
                                new SimpleTrigger(Triggers.RECEIVED_REPLY)));

        // Sleeping -> RunningLua
        _sm.PushTransitionRule(new TransitionRule(
                                DialogueState.Sleeping.ToString(),
                                DialogueState.RunningLua.ToString(),
                                new SimpleTrigger(Triggers.SLEEP_COMPLETE)));

    }
    protected virtual void Custom(DynValue argument){}

    // API
    public void Hurry(){
        DialogueState currentState = State;
        if(currentState == DialogueState.RenderingText){
            _visibility = _text.Length;
            _sm.Trigger(Triggers.RENDER_COMPLETE);
        }
        else if(currentState == DialogueState.Sleeping){
            _sm.UpdateParameter("sleepTime", 0.0f);
            _sm.Trigger(Triggers.SLEEP_COMPLETE);
        }
    }

    public void Ok(){
        _sm.Trigger(Triggers.RECEIVED_OK);
    }

    public void Reply(int reply){
        _lua.Globals["__bridge_value"] = reply;
        _sm.Trigger(Triggers.RECEIVED_REPLY);
    }

    public void Goto(int code){
        if(code == 0){
            throw new DialogueException("Jumping to code 0 is not allowed.", 13);
        }
        DynValue function = _lua.DoString(_ldg.Entries[code]);
        _coroutine = _lua.CreateCoroutine(function);
        _sm.ForceState(DialogueState.RunningLua.ToString());
    }

    public void Restart(){
        Goto(InitialCode);
    }

    public void HotReload(string ldgSource){
        _ldg = LDG.FromString(ldgSource);
    }
    
    public void Register<T>(string name, T value){
        _lua.Globals[name] = value;
    }

    public DynValue GetLuaValue(string name){
        return _lua.Globals.Get(name);
    }

    public Dictionary<int, string> Options;

    public string Text {
        get {
            int length = (int) System.Math.Min(_text.Length, _visibility);
            return _text.Substring(0, length);
        }
        private set {}
    }

    public DialogueState State {
        get {
            BaseState? currentState = _sm.CurrentState;
            if(currentState is null){
                throw new DialogueException("For some reason, DialogueState is null. This should never happen.", 14);
            }
            if(currentState is SimpleState<DialogueState>){
                return (currentState as SimpleState<DialogueState>)!.State;
            } else {
                throw new DialogueException("For some reason, DialogueState is not the correct type. This should never happen.", 15);
            }
        }
        private set {}
    }
    
    public bool AllowEmptyMessages = true;
    public bool PauseAfterSay = true;
    public bool SkipSpecial = false;
    public int InitialCode = 1;
    public float TextSpeed = 1;

    // Component

    public override void OnAddedToEntity(){
        DynValue initFunc = _lua.DoString(_ldg.Entries[0]);
        _lua.Call(initFunc);

        _lua.Globals["__set_text_speed"] = (Action<float>) _setTextSpeed;
        _lua.Globals["__set_pause_after_say"] = (Action<bool>) _setPauseAfterSay;
        _lua.Globals["__set_initial_code"] = (Action<int>) _setInitialCode;
        _lua.Globals["__set_skip_special"] = (Action<bool>) _setSkipSpecial;

        DynValue function = _lua.DoString(_ldg.Entries[InitialCode]);
        _coroutine = _lua.CreateCoroutine(function);
    }
    
    public void Update(){
        _sm.Update();
    }

    // Lua interaction methods

    private void _setTextSpeed(float value){
        TextSpeed = value;
    }
    private void _setPauseAfterSay(bool value){
        PauseAfterSay = value;
    }
    private void _setInitialCode(int code){
        InitialCode = code;
    }
    private void _setSkipSpecial(bool value){
        SkipSpecial = value;
    }

    // Private members
    private string _text;
    private LDG _ldg;
    private DynValue _coroutine;
    private Script _lua;
    private StateMachine _sm;

    private float _timeBuffer = 0;
    private long _visibility;
}
