#nullable enable
using System.Collections.Generic;
using System;

public class Message {
    public Message(string text, object? content = null){
        Text = text;
        Content = content;
    }
    public string Text;
    public object? Content;
}

public interface IMessageSubscriber {
    void ProcessMessage(Message message);
}

public class MessageBroker {
    class TaggedSubscriber {
        public TaggedSubscriber(IMessageSubscriber subscriber, long id){
            Subscriber = subscriber;
            Id = id;
        }
        public IMessageSubscriber Subscriber;
        public long Id;
    }
    public static MessageBroker? Instance;
    public MessageBroker() {
        _subscribers = new Dictionary<string, List<TaggedSubscriber>>();
        _id = 1;
    }

    public static void SendMessage(string tag, string text, object? content, MessageBroker? broker = null) {
        SendMessage(tag, new Message(text, content), broker);
    }
    public static void SendMessage(string tag, Message message, MessageBroker? broker = null) {
        if(broker is null){
            broker = MessageBroker.Instance;
        }

        if(broker is null){
            throw new NullReferenceException("Message broker has not been instantiated.");
        }

        broker._sendMessage(tag, message);

    }
    public static long Register(IMessageSubscriber subscriber, MessageBroker? broker = null){
        return Register("", subscriber, broker);
    }
    public static long Register(string tag, IMessageSubscriber subscriber, MessageBroker? broker = null){
        if(broker is null){
            broker = MessageBroker.Instance;
        }
        if(broker is null){
            throw new NullReferenceException("Message broker has not been instantiated.");
        }

        return broker._register(tag, subscriber);

    }

    public static void Deregister(long subId, MessageBroker? broker = null){
        if(broker is null){
            broker = MessageBroker.Instance;
        }

        if(broker is null){
            throw new NullReferenceException("Message broker has not been instantiated.");
        }

        broker._deregister(subId);
    }

    private long _register(string tag, IMessageSubscriber subscriber){

        long subId = ++_id;
        TaggedSubscriber tsub = new TaggedSubscriber(subscriber, subId);

        if(_subscribers.ContainsKey(tag)) {
            _subscribers[tag].Add(tsub);
        } else {
            _subscribers[tag] = new List<TaggedSubscriber>();
            _subscribers[tag].Add(tsub);
        }
        return subId;
    }

    private void _deregister(long subId){
        foreach( var (_, subList) in _subscribers){
            subList.RemoveAll(tsub => tsub.Id == subId);
        }
    }

    private void _sendMessage(string tag, Message message){
        if(_subscribers.ContainsKey(tag)){
            foreach (var subscriber in _subscribers[tag]){
                subscriber.Subscriber.ProcessMessage(message);
            }
        }

        if(_subscribers.ContainsKey("") && tag != ""){
            foreach (var subscriber in _subscribers[""]){
                subscriber.Subscriber.ProcessMessage(message);
            }

        }
    }

    long _id;
    private Dictionary<string, List<TaggedSubscriber>> _subscribers;
}

public class MessageSubscriber : Nez.Component, IMessageSubscriber {
    public MessageSubscriber(string tag, Action<Message> handler) {
        _tag = tag;
        _handler = handler;
        _id = 0;
    }
    public MessageSubscriber(Action<Message> handler) {
        _tag = "";
        _handler = handler;
        _id = 0;
    }
    public void ProcessMessage(Message message){
        _handler(message);
    }

    public override void OnAddedToEntity(){
        _id = MessageBroker.Register(_tag, this);
    }

    public override void OnRemovedFromEntity(){
        MessageBroker.Deregister(_id);
    }

    private long _id;
    private string _tag;
    private Action<Message> _handler;
}
