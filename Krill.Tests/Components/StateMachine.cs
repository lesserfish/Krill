using Nez.Extension;

public class SM {

    enum DoorState {
        Closed,
        Open
    };
    [Fact]
    public void Unit1(){
        // A simple door with two locks.
        // Open when both locks are open
        // Close when both doors are closed
        StateMachine sm = new StateMachine();
        sm.PushState(new SimpleState<DoorState>(DoorState.Closed), true);
        sm.PushState(new SimpleState<DoorState>(DoorState.Open));
        sm.PushTransitionRule(new TransitionRule("Closed", "Open", Condition.Unset("Lock1") & Condition.Unset("Lock2")));
        sm.PushTransitionRule(new TransitionRule("Open", "Closed", Condition.Set("Lock1") | Condition.Set("Lock2")));
        sm.NewParameter("Lock1", true);
        sm.NewParameter("Lock2", true);

        // Initial State

        Assert.Equal(DoorState.Closed, (sm.CurrentState as SimpleState<DoorState>)!.State);

        // Open Lock 1
        sm.UpdateParameter("Lock1", false).Update();
        Assert.Equal(DoorState.Closed, (sm.CurrentState as SimpleState<DoorState>)!.State);

        // Open Lock 2
        sm.UpdateParameter("Lock2", false).Update();
        Assert.Equal(DoorState.Open, (sm.CurrentState as SimpleState<DoorState>)!.State);

        // Close Lock 1
        sm.UpdateParameter("Lock1", true).Update();
        Assert.Equal(DoorState.Closed, (sm.CurrentState as SimpleState<DoorState>)!.State);


    }
    
    class SimpleMusic : IState {
        public SimpleMusic(int duration) : base("Playing") {
            _timeRemaining = duration;
            _duration = duration;
        }
        public override void Update(StateMachine owner){
            // Play one second of music and update the environment
            _timeRemaining = Math.Max(_timeRemaining - 1, 0);
            owner.UpdateParameter("TimeRemaining", _timeRemaining);
        }
        public override void OnStart(StateMachine owner){
            if(owner.PreviousState != null){
                // If the previous state was Stopped, start from scratch.
                if(owner.PreviousState.Name == "Stopped"){
                    _timeRemaining = _duration;
                }
            }
            owner.UpdateParameter("TimeRemaining", _timeRemaining);
        }
        private int _timeRemaining;
        private int _duration;
    }

    [Fact]
    public void Unit2(){
        // A simple music player
        // It can be paused, stopped, or playing a 5 second duration song
        // It automatically stops when the music ends;
        
        StateMachine sm = new StateMachine();
        sm.PushState(new StringState("Stopped"), true);
        sm.PushState(new StringState("Paused"));
        sm.PushState(new SimpleMusic(5));
        sm.PushTransitionRule(new TransitionRule("Stopped", "Playing", Trigger.Simple("Play")));
        sm.PushTransitionRule(new TransitionRule("Playing", "Stopped", Trigger.Simple("Stop")));
        sm.PushTransitionRule(new TransitionRule("Paused" , "Playing", Trigger.Simple("Play")));
        sm.PushTransitionRule(new TransitionRule("Playing", "Paused" , Trigger.Simple("Pause")));
        sm.PushTransitionRule(new TransitionRule("Paused" , "Stopped", Trigger.Simple("Stop")));
        sm.PushTransitionRule(new TransitionRule("Playing", "Stopped", Condition.IntEq("TimeRemaining", 0)));


        // We start with the music player stopped
        Assert.Equal("Stopped", sm.CurrentState!.Name);
        
        // We then hit play
        sm.Trigger("Play");
        Assert.Equal( "Playing", sm.CurrentState!.Name);
        Assert.Equal( 5, (int)sm.GetParameter("TimeRemaining")!);

        // It plays for three seconds
        sm.Update();
        sm.Update();
        sm.Update();
        Assert.Equal("Playing", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("TimeRemaining")!);

        // We pause the music
        sm.Trigger("Pause");
        Assert.Equal("Paused", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("TimeRemaining")!);

        // We resume the music
        sm.Trigger("Play");
        Assert.Equal("Playing", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("TimeRemaining")!);

        // We play until completion
        sm.Update();
        sm.Update();
        Assert.Equal("Stopped", sm.CurrentState!.Name);
        Assert.Equal(0, (int)sm.GetParameter("TimeRemaining")!);

        // We start again
        sm.Trigger("Play");
        Assert.Equal("Playing", sm.CurrentState!.Name);
        Assert.Equal(5, (int)sm.GetParameter("TimeRemaining")!);

        // It plays for three seconds
        sm.Update();
        sm.Update();
        sm.Update();
        Assert.Equal("Playing", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("TimeRemaining")!);

        // We stop the music
        sm.Trigger("Stop");
        Assert.Equal("Stopped", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("TimeRemaining")!);

        // We start again
        sm.Trigger("Play");
        Assert.Equal("Playing", sm.CurrentState!.Name);
        Assert.Equal(5, (int)sm.GetParameter("TimeRemaining")!);

    }

    [Fact]
    public void Unit3(){
        // Heater
        // Turns on when temperature is less than 18 Celsius
        StateMachine sm = new StateMachine();
        sm.PushState(new StringState("On"));
        sm.PushState(new StringState("Off"), true);
        sm.PushTransitionRule(new TransitionRule("On", "Off", Condition.DoubleGe("Temperature", 18)));
        sm.PushTransitionRule(new TransitionRule("Off", "On", Condition.DoubleLt("Temperature", 18)));
        sm.NewParameter("Temperature", 20);


        // We start with 20 degrees
        sm.Update();
        Assert.Equal("Off", sm.CurrentState!.Name);

        // It decreases to 15
        sm.UpdateParameter<double>("Temperature", 15).Update();
        Assert.Equal("On", sm.CurrentState!.Name);

        // It decreases to 12
        sm.UpdateParameter<double>("Temperature", 12).Update();
        Assert.Equal("On", sm.CurrentState!.Name);

        // It increases to 22
        sm.UpdateParameter<double>("Temperature", 22).Update();
        Assert.Equal("Off", sm.CurrentState!.Name);
    }
    
    class ElevatorMoving : IState {
        public ElevatorMoving() : base ("Moving") {}

        public override void OnStart(StateMachine owner){
            int floor = (int)owner.GetParameter("Floor")!;
            List<int> requests = (owner.GetParameter("Requests") as List<int>)!;
            int target = requests[0];

            if(floor == target){
                owner.UpdateParameter("Arrived", true);
            } else {
                owner.UpdateParameter("Arrived", false);
            }
        }

        public override void Update(StateMachine owner){
            int floor = (int)owner.GetParameter("Floor")!;
            List<int> requests = (owner.GetParameter("Requests") as List<int>)!;

            if(requests.Count == 0){
                owner.UpdateParameter("Arrived", true);
            } else {
                int target = requests[0];
                int new_floor = floor + Math.Sign(target - floor);

                owner.UpdateParameter("Floor", new_floor);

                if(target == new_floor){
                    requests.RemoveAt(0);
                    owner.UpdateParameter("Arrived", true);

                }
            }
        }
    }

    class ElevatorRequest : ICondition {
        public ElevatorRequest(bool accept) {
            _accept = accept;
        }
        public override bool Trigger(string trigger, object value, IState current_state, Dictionary<string, object> environment){
            if(trigger == "Request"){
                int new_request = (int)value;
                List<int> requests = (environment["Requests"] as List<int>)!;
                requests.Add(new_request);
                return _accept;
            }
            return false;
        }
        private bool _accept;
    }

    class ExistingRequest : ICondition {
        public override bool Accept(IState current_state, Dictionary<string, object> environment){
            List<int> requests = (environment["Requests"] as List<int>)!;
            return requests.Count > 0;
        }
    }
    [Fact]
    public void Unit4(){
        // Elevator
        StateMachine sm = new StateMachine();
        sm.PushState(new ElevatorMoving());
        sm.PushState(new StringState("Stopped"), true);
        sm.PushTransitionRule(new TransitionRule("Stopped", "Moving", new ElevatorRequest(true)));
        sm.PushTransitionRule(new TransitionRule("Stopped", "Moving", new ExistingRequest()));
        sm.PushTransitionRule(new TransitionRule("Moving", "Stopped", Condition.Set("Arrived")));
        sm.PushTransitionRule(new TransitionRule("Moving", "Moving", new ElevatorRequest(false)));
        sm.NewParameter("Arrived", true);
        sm.NewParameter("Floor", 0);
        sm.NewParameter("Requests", new List<int>());


        // We start with the elevator stopped
        Assert.Equal("Stopped", sm.CurrentState!.Name);
        
        // We get a request in floor 5
        sm.Trigger("Request", 5);
        Assert.Equal( "Moving", sm.CurrentState!.Name);
        Assert.Equal(0, (int)sm.GetParameter("Floor")!);

        // Two iterations pass
        sm.Update(); // Floor 1
        sm.Update(); // Floor 2
        Assert.Equal( "Moving", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("Floor")!);

        // We arrive at the floor
        sm.Update(); // Floor 3
        sm.Update(); // Floor 4
        sm.Update(); // Floor 5

        Assert.Equal("Stopped", sm.CurrentState!.Name);
        Assert.Equal(5, (int)sm.GetParameter("Floor")!);

        // We get a request in floor 2
        sm.Trigger("Request", 2);
        Assert.Equal( "Moving", sm.CurrentState!.Name);
        Assert.Equal(5, (int)sm.GetParameter("Floor")!);

        // The elevator moves two floors
        sm.Update(); // Floor 4
        sm.Update(); // Floor 3
        Assert.Equal( "Moving", sm.CurrentState!.Name);
        Assert.Equal(3, (int)sm.GetParameter("Floor")!);

        // The elevator gets a new request at floor 4
        sm.Trigger("Request", 4);
        Assert.Equal( "Moving", sm.CurrentState!.Name);
        Assert.Equal(3, (int)sm.GetParameter("Floor")!);

        // The elevator finishes its current goal 
        sm.Update(); // Floor 2
        Assert.Equal("Stopped", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("Floor")!);

        // It immediately starts going to floor 4
        sm.Update();
        Assert.Equal("Moving", sm.CurrentState!.Name);
        Assert.Equal(2, (int)sm.GetParameter("Floor")!);

        // It reaches floor 4
        sm.Update(); // Floor 3
        sm.Update(); // Floor 4
        Assert.Equal( "Stopped", sm.CurrentState!.Name);
        Assert.Equal(4, (int)sm.GetParameter("Floor")!);

    }

    class Counter : IState {
        public Counter(Action<int> setter) : base("Counting") {
            _setter = setter;
            _value = 0;
        }
        public override void OnStart(StateMachine owner){
            if(owner.PreviousState!.Name != "Paused"){
                _value = 0;
                _setter(_value);
            }
        }
        public override void Update(StateMachine owner){
            _value++;
            _setter(_value);
        }
        private Action<int> _setter;
        private int _value;
    }
    [Fact]
    public void Unit5(){
        int counter = 0;
        Action<int> setter = (value) => {counter = value;};

        StateMachine sm = new StateMachine();
        sm.PushState(new Counter(setter));
        sm.PushState(new StringState("Paused"), true);
        sm.PushTransitionRule(new TransitionRule("Counting", "Paused", Trigger.Simple("Pause")));
        sm.PushTransitionRule(new TransitionRule("Paused", "Counting", Trigger.Simple("Resume")));
        sm.PushTransitionRule(new TransitionRule("Counting", "Counting", Trigger.Simple("Restart")));
        Assert.Equal(0, counter);

        sm.Trigger("Resume");
        sm.Update();
        sm.Update();
        sm.Update();
        Assert.Equal(3, counter);

        sm.Trigger("Pause");
        sm.Update();
        sm.Update();
        sm.Update();
        Assert.Equal(3, counter);

        sm.Trigger("Resume");
        sm.Update();
        sm.Update();
        sm.Update();
        Assert.Equal(6, counter);

        sm.Trigger("Restart");
        Assert.Equal(0, counter);


    }

}
