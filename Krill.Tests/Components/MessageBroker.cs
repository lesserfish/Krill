using Nez.Extension;

class MBHelper : IMessageSubscriber {
    public MBHelper(Action<Message> handler){
        Handler = handler;
    }
    public void ProcessMessage(Message message){
        Handler(message);
    }
    public Action<Message> Handler;
}

public class MB {
    [Fact]
    public void Unit1(){ 
        MessageBroker broker = new MessageBroker();
        int state = 0;

        // Update state when you receive a message 'UpdateState'
        MBHelper subscriber = new MBHelper((message) => {if(message.Text == "UpdateState") {state = 1;}});
        MessageBroker.Register("test", subscriber, broker);

        Assert.Equal(0, state);

        MessageBroker.SendMessage("test", "UpdateState", null, broker);

        Assert.Equal(1, state);
    }

    [Fact]
    public void Unit2(){ 
        MessageBroker broker = new MessageBroker();
        int state = 0;

        // Update state when you receive a message 'UpdateState'
        MBHelper subscriber = new MBHelper((message) => {if(message.Text == "UpdateState") {state = 1;}});
        MessageBroker.Register(subscriber, broker);

        Assert.Equal(0, state);

        MessageBroker.SendMessage("test", "UpdateState", null, broker);

        Assert.Equal(1, state);
    }

    [Fact]
    public void Unit3(){ 
        MessageBroker broker = new MessageBroker();
        int state = 0;

        // Update state when you receive a message 'UpdateState'
        MBHelper subscriber = new MBHelper((message) => {if(message.Text == "IncreaseState") {state += 1;}});
        long id = MessageBroker.Register(subscriber, broker);
        Assert.Equal(0, state);

        MessageBroker.SendMessage("test", "IncreaseState", null, broker);
        Assert.Equal(1, state);

        MessageBroker.Deregister(id, broker);
        MessageBroker.SendMessage("test", "IncreaseState", null, broker);
        Assert.Equal(1, state);
    }


    [Fact]
    public void Unit4(){ 
        MessageBroker broker = new MessageBroker();
        int state = 0;

        // Update state when you receive a message 'UpdateState'
        MBHelper subscriber = new MBHelper((message) => {state = (int)message.Content!;});
        MessageBroker.Register("test", subscriber, broker);

        Assert.Equal(0, state);

        MessageBroker.SendMessage("test", "UpdateState", 27, broker);

        Assert.Equal(27, state);
    }

}
