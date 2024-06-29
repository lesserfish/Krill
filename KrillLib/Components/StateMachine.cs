#nullable enable
using System;
using System.Collections.Generic;

namespace Nez.Extension;

public class BaseState {
    public BaseState(string name){
        Name = name;
    }
    public virtual void Update(StateMachine owner){}
    public virtual void OnStart(StateMachine owner){}
    public virtual void OnExit(StateMachine owner){}
    public string Name {get; private set;}
}

public abstract class ACondition {
    public virtual bool Accept(BaseState current_state, Dictionary<string, object> environment){
        return false;
    }
    public virtual bool Trigger(string trigger, object value, BaseState current_state, Dictionary<string, object> environment){
        return false;
    }
    public virtual bool Trigger(string trigger, BaseState current_state, Dictionary<string, object> environment){
        return false;
    }
    public static ACondition operator |(ACondition a, ACondition b)
    {
        return new OrCondition(a, b);
    }
    public static ACondition operator &(ACondition a, ACondition b)
    {
        return new AndCondition(a, b);
    }
}

public class TransitionRule {
    public TransitionRule(string source, string target, ACondition condition){
        Source = source;
        Target = target;
        _condition = condition;
    }
    public bool Accept(BaseState current_state, Dictionary<string, object> environment) {
        return _condition.Accept(current_state, environment);
    }
    public bool Trigger(string trigger, BaseState current_state, Dictionary<string, object> environment) {
        return _condition.Trigger(trigger,current_state, environment);
    }

    public bool Trigger(string trigger, object value, BaseState current_state, Dictionary<string, object> environment) {
        return _condition.Trigger(trigger, value, current_state, environment);
    }

    public string Source {get; private set;}
    public string Target {get; private set;}
    private ACondition _condition;

}

public class StateMachine : Nez.Component, Nez.IUpdatable {
    public StateMachine(){
        _states = new Dictionary<string, BaseState>();
        _parameters = new Dictionary<string, object>();
        _transitions = new Dictionary<string, List<TransitionRule>>();
        CurrentState = null;
        PreviousState = null;
    }

    public StateMachine PushState(BaseState state, bool current_state = false){
        if(_states.ContainsKey(state.Name)){
            throw new ArgumentException("State machine already has a state with name '{0}'", state.Name);
        } 
        _states.Add(state.Name, state);
        _transitions.Add(state.Name, new List<TransitionRule>());

        if(current_state){
            CurrentState = state;
            CurrentState.OnStart(this);
        }


        return this;
    }

    public StateMachine PushTransitionRule(TransitionRule transition) {
        string source = transition.Source;
        string target = transition.Target;

        if(!_states.ContainsKey(source)){
            throw new ArgumentException("State machine does not contain a state with name '{0}'", source);
        }
        if(!_states.ContainsKey(target)){
            throw new ArgumentException("State machine does not contain a state with name '{0}'", target);
        }

        _transitions[source].Add(transition);

        return this;
    }

    public StateMachine NewParameter<T>(string name, T value){
        if(value == null){
            throw new ArgumentException("Attempted to add a null parameter to state machine", name);
        }
        if(_parameters.ContainsKey(name)){
            throw new ArgumentException("State machine already has a parameter with name '{0}'", name);
        }
        _parameters.Add(name, value);

        return this;
    }

    public StateMachine UpdateParameter<T>(string name, T value) {
        if(value == null){
            throw new ArgumentException("Attempted to add a null parameter to state machine", name);
        }

        if(_parameters.ContainsKey(name)){
            _parameters[name] = value;
        } else {
            _parameters.Add(name, value);
        }

        return this;
    }

    public object? GetParameter(string name){
        if(_parameters.ContainsKey(name)){
            return _parameters[name];
        }
        return null;
    }

    public StateMachine ForceState(string name) {
        if(!_states.ContainsKey(name)){
            throw new ArgumentException("State machine does not contain a state with name '{0}'", name);
        }

        BaseState newState = _states[name];

        PreviousState = CurrentState;
        CurrentState = newState;
        
        if(PreviousState != null){
            PreviousState.OnExit(this);
        }

        CurrentState.OnStart(this);

        return this;
    }

    public void Update() {
        if(CurrentState == null){
            throw new ArgumentException("Current state is null");
        }
        CurrentState.Update(this);
        foreach (TransitionRule transition in _transitions[CurrentState.Name]){
            if(transition.Accept(CurrentState, _parameters)){
                string target = transition.Target;

                if(!_states.ContainsKey(transition.Target)){
                    throw new ArgumentException("State machine does not contain a state with name '{0}'", target);
                }

                BaseState newState = _states[target];

                PreviousState = CurrentState;
                CurrentState = newState;

                PreviousState.OnExit(this);
                CurrentState.OnStart(this);

                return;
            }
        }
    }

    public StateMachine Trigger() {
        if(CurrentState == null){
            throw new ArgumentException("Current state is null");
        }
        foreach (TransitionRule transition in _transitions[CurrentState.Name]){
            if(transition.Accept(CurrentState, _parameters)){
                string target = transition.Target;

                if(!_states.ContainsKey(transition.Target)){
                    throw new ArgumentException("State machine does not contain a state with name '{0}'", target);
                }

                BaseState newState = _states[target];

                PreviousState = CurrentState;
                CurrentState = newState;

                PreviousState.OnExit(this);
                CurrentState.OnStart(this);

                return this;
            }
        }
        return this;
    }
    public StateMachine Trigger(string trigger){
        if(CurrentState == null){
            throw new NullReferenceException("Current state is null");
        }
        foreach (TransitionRule transition in _transitions[CurrentState.Name]){
            if(transition.Trigger(trigger, CurrentState, _parameters)){
                string target = transition.Target;

                if(!_states.ContainsKey(transition.Target)){
                    throw new NullReferenceException(String.Format("State machine does not contain a state with name '{0}'", target));
                }

                BaseState newState = _states[target];

                PreviousState = CurrentState;
                CurrentState = newState;

                PreviousState.OnExit(this);
                CurrentState.OnStart(this);

                return this;
            }
        }
        return this;
    }

    public StateMachine Trigger(string trigger, object value){
        if(CurrentState == null){
            throw new NullReferenceException("Current state is null");
        }
        foreach (TransitionRule transition in _transitions[CurrentState.Name]){
            if(transition.Trigger(trigger, value,CurrentState, _parameters)){
                string target = transition.Target;

                if(!_states.ContainsKey(transition.Target)){
                    throw new NullReferenceException(String.Format("State machine does not contain a state with name '{0}'", target));
                }

                BaseState newState = _states[target];

                PreviousState = CurrentState;
                CurrentState = newState;

                PreviousState.OnExit(this);
                CurrentState.OnStart(this);

                return this;
            }
        }
        return this;

    }

    public StateMachine Clear(){
        _states.Clear();
        _parameters.Clear();
        _transitions.Clear();
        CurrentState = null;
        return this;
    }

    // Should this function be removed?
    public StateMachine RemoveState(string name){
        if(_states.ContainsKey(name)){
            _states.Remove(name);
            _transitions.Remove(name);
            foreach (string state in _transitions.Keys){
                _transitions[state].RemoveAll(transition => transition.Target == name);
            }
            if(CurrentState != null && CurrentState.Name == name){
                CurrentState = null;
            }
        }
        return this;
    }

    public override void OnRemovedFromEntity(){
        Clear();
    }

    public BaseState? CurrentState {get; private set;}
    public BaseState? PreviousState {get; private set;}
    private Dictionary<string, BaseState> _states;
    private Dictionary<string, object> _parameters;
    private Dictionary<string, List<TransitionRule>> _transitions;
}

// States


public class SimpleState<T> : BaseState, IEquatable<SimpleState<T>> where T : Enum {
    public SimpleState(T state) : base(state.ToString()) {
        State = state;
    }
    public bool Equals(SimpleState<T>? other){
        if(other == null){
            return false;
        }
        return State.Equals(other.State);
    }
    public T State {get; private set;} 
}

public class StringState : BaseState, IEquatable<StringState> {
    public StringState(string state) : base(state){}
    public bool Equals(StringState? other){
        if(other == null){
            return false;
        }
        return Name.Equals(other.Name);
    }
}
// Conditions
public class OrCondition : ACondition {
    public OrCondition(ACondition A, ACondition B){
        _conditionA = A;
        _conditionB = B;
    }

    public override bool Accept(BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Accept(current_state, environment) | _conditionB.Accept(current_state, environment);
    }
    public override bool Trigger(string trigger, object value, BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Trigger(trigger, value, current_state, environment) | _conditionB.Trigger(trigger, value, current_state, environment);
    }
    public override bool Trigger(string trigger, BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Trigger(trigger, current_state, environment) | _conditionB.Trigger(trigger, current_state, environment);
    }

    private ACondition _conditionA;
    private ACondition _conditionB;
}

public class AndCondition : ACondition {
    public AndCondition(ACondition A, ACondition B){
        _conditionA = A;
        _conditionB = B;
    }

    public override bool Accept(BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Accept(current_state, environment) & _conditionB.Accept(current_state, environment);
    }
    public override bool Trigger(string trigger, object value, BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Trigger(trigger, value, current_state, environment) & _conditionB.Trigger(trigger, value, current_state, environment);
    }
    public override bool Trigger(string trigger, BaseState current_state, Dictionary<string, object> environment){
        return _conditionA.Trigger(trigger, current_state, environment) & _conditionB.Trigger(trigger, current_state, environment);
    }

    private ACondition _conditionA;
    private ACondition _conditionB;
}

public class EqualCondition<T> : ACondition where T : IEquatable<T> {
    public EqualCondition(string name, T value){
        _value = value;
        _name = name;
    }

    public override bool Accept(BaseState current_state, Dictionary<string, object> environment){
        if(!environment.ContainsKey(_name)){
            return false;
        }
        object value = environment[_name];
        if(value == null){
            return false;
        }
        if(!(value is T)){
            return false;
        }
        T tvalue = (T) value;
        return _value.Equals(tvalue);
    }

    private string _name;
    private T _value;

}

public class CompareCondition<T> : ACondition where T : IComparable {
    public CompareCondition(string name, T value, int check){
        _name = name;
        _value = value;
        _check = check;
    }

    public override bool Accept(BaseState current_state, Dictionary<string, object> environment){
        if(!environment.ContainsKey(_name)){
            return false;
        }
        object value = environment[_name];
        if(value == null){
            return false;
        }
        if(!(value is T)){
            return false;
        }
        T tvalue = (T) value;
        return tvalue.CompareTo(_value) == _check;
    }    

    private string _name;
    private T _value;
    private int _check;
}

public class Condition {
    public static ACondition IntLt(string name, int value){
        return new CompareCondition<int>(name, value, -1);
    }
    public static ACondition DoubleLt(string name, double value){
        return new CompareCondition<double>(name, value, -1);
    }
    public static ACondition FloatLt(string name, float value){
        return new CompareCondition<float>(name, value, -1);
    }

    public static ACondition IntGt(string name, int value){
        return new CompareCondition<int>(name, value, 1);
    }
    public static ACondition DoubleGt(string name, double value){
        return new CompareCondition<double>(name, value, 1);
    }
    public static ACondition FloatGt(string name, float value){
        return new CompareCondition<float>(name, value, 1);
    }

    public static ACondition IntLe(string name, int value){
        return (new CompareCondition<int>(name, value, -1)) | (new CompareCondition<int>(name, value, 0));
    }
    public static ACondition DoubleLe(string name, double value){
        return (new CompareCondition<double>(name, value, -1)) | (new CompareCondition<double>(name, value, 0));
    }
    public static ACondition FloatLe(string name, float value){
        return (new CompareCondition<float>(name, value, -1)) | (new CompareCondition<float>(name, value, 0));
    }

    public static ACondition IntGe(string name, int value){
        return (new CompareCondition<int>(name, value, 1)) | (new CompareCondition<int>(name, value, 0));
    }
    public static ACondition DoubleGe(string name, double value){
        return (new CompareCondition<double>(name, value, 1)) | (new CompareCondition<double>(name, value, 0));
    }
    public static ACondition FloatGe(string name, float value){
        return (new CompareCondition<float>(name, value, 1)) | (new CompareCondition<float>(name, value, 0));
    }


    public static ACondition IntEq(string name, int value){
        return new EqualCondition<int>(name, value);
    }
    public static ACondition DoubleEq(string name, double value){
        return new EqualCondition<double>(name, value);
    }
    public static ACondition FloatEq(string name, float value){
        return new EqualCondition<float>(name, value);
    }
    public static ACondition StrEq(string name, string value){
        return new EqualCondition<string>(name, value);
    }
    public static ACondition BoolEq(string name, bool value){
        return new EqualCondition<bool>(name, value);
    }
    public static ACondition Set(string name){
        return new EqualCondition<bool>(name, true);
    }
    public static ACondition Unset(string name){
        return new EqualCondition<bool>(name, false);
    }
}

public class SimpleTrigger : ACondition {
    public SimpleTrigger(string trigger){
        _trigger = trigger;
    }

    public override bool Trigger(String trigger, BaseState current_state, Dictionary<string, object> environment){
        if(trigger != _trigger){
            return false;
        }
        return true;
    }

    private string _trigger;
}

public class EqualTrigger<T> : ACondition where T : IEquatable<T> {
    public EqualTrigger(string trigger, T value){
        _value = value;
        _trigger = trigger;
    }

    public override bool Trigger(String trigger, Object value, BaseState current_state, Dictionary<string, object> environment){
        if(trigger != _trigger){
            return false;
        }
        if(value == null){
            return false;
        }
        if(!(value is T)){
            return false;
        }
        T tvalue = (T) value;
        return tvalue.Equals(_value);
    }

    private string _trigger;
    private T _value;

}

public class CompareTrigger<T> : ACondition where T : IComparable<T> {
    public CompareTrigger(string trigger, T value, int check){
        _value = value;
        _trigger = trigger;
        _check = check;
    }

    public override bool Trigger(String trigger, Object value, BaseState current_state, Dictionary<string, object> environment){
        if(trigger != _trigger){
            return false;
        }
        if(value == null){
            return false;
        }
        if(!(value is T)){
            return false;
        }
        T tvalue = (T) value;
        return tvalue.CompareTo(_value) == _check;
    }

    private string _trigger;
    private T _value;
    private int _check;
}

public class Trigger {
    public static ACondition IntLt(string name, int value){
        return new CompareTrigger<int>(name, value, -1);
    }
    public static ACondition DoubleLt(string name, double value){
        return new CompareTrigger<double>(name, value, -1);
    }
    public static ACondition FloatLt(string name, float value){
        return new CompareTrigger<float>(name, value, -1);
    }

    public static ACondition IntGt(string name, int value){
        return new CompareTrigger<int>(name, value, 1);
    }
    public static ACondition DoubleGt(string name, double value){
        return new CompareTrigger<double>(name, value, 1);
    }
    public static ACondition FloatGt(string name, float value){
        return new CompareTrigger<float>(name, value, 1);
    }

    public static ACondition IntLe(string name, int value){
        return (new CompareTrigger<int>(name, value, -1)) | (new CompareTrigger<int>(name, value, 0));
    }
    public static ACondition DoubleLe(string name, double value){
        return (new CompareTrigger<double>(name, value, -1)) | (new CompareTrigger<double>(name, value, 0));
    }
    public static ACondition FloatLe(string name, float value){
        return (new CompareTrigger<float>(name, value, -1)) | (new CompareTrigger<float>(name, value, 0));
    }

    public static ACondition IntGe(string name, int value){
        return (new CompareTrigger<int>(name, value, 1)) | (new CompareTrigger<int>(name, value, 0));
    }
    public static ACondition DoubleGe(string name, double value){
        return (new CompareTrigger<double>(name, value, 1)) | (new CompareTrigger<double>(name, value, 0));
    }
    public static ACondition FloatGe(string name, float value){
        return (new CompareTrigger<float>(name, value, 1)) | (new CompareTrigger<float>(name, value, 0));
    }


    public static ACondition IntEq(string name, int value){
        return new EqualTrigger<int>(name, value);
    }
    public static ACondition DoubleEq(string name, double value){
        return new EqualTrigger<double>(name, value);
    }
    public static ACondition FloatEq(string name, float value){
        return new EqualTrigger<float>(name, value);
    }
    public static ACondition StrEq(string name, string value){
        return new EqualTrigger<string>(name, value);
    }
    public static ACondition BoolEq(string name, bool value){
        return new EqualTrigger<bool>(name, value);
    }
    public static ACondition Set(string name){
        return new EqualTrigger<bool>(name, true);
    }
    public static ACondition Unset(string name){
        return new EqualTrigger<bool>(name, false);
    }

    public static ACondition Simple(string name){
        return new SimpleTrigger(name);
    }

}
