using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez.Extension;
using System.IO;
using System;

namespace Demo;

public static class DialogueLoader{
    public static DialogueManager Load(){
        string exePath = System.Reflection.Assembly.GetExecutingAssembly().Location;
        string luaInitPath = Path.Combine(Path.GetDirectoryName(exePath), "Content", "init_dialogue.lua");
        string luaSourcePath = Path.Combine(Path.GetDirectoryName(exePath), "Content", "dialogue_example.ldg");

        string luaInit = File.ReadAllText(luaInitPath);
        string ldgSource = File.ReadAllText(luaSourcePath);

        return new DialogueManager(ldgSource, luaInit);
    }
}
public class SimpleComponent : Nez.Component, Nez.IUpdatable {
    public SimpleComponent(Action update){
        _update = update;
    }
    public void Update(){
        _update();
    }
    
    Action _update;
}
public class DialogueEntity : Nez.Entity {
    public override void OnAddedToScene() {
        Transform.Position = new Vector2(30, 50);
        Transform.Scale = 3 * Vector2.One;
        text = new Nez.TextComponent();
        dialogue = DialogueLoader.Load();
        dialogue.TextSpeed = 10.0f;
        dialogue.PauseAfterSay = false;
        AddComponent(dialogue);
        AddComponent(text);
        AddComponent(new SimpleComponent(() => {
                        if(Nez.Input.IsKeyPressed(Keys.Space))
                            HandleKeyDown(Keys.Space);
                        if(Nez.Input.IsKeyPressed(Keys.Enter))
                            HandleKeyDown(Keys.Enter);
                        RenderText();
                    }));
        
    }
    void HandleKeyDown(Keys key){
        System.Console.WriteLine("Received keydown {0}, key");
        if(key == Keys.Space){
            if(dialogue.State == DialogueState.Paused){
                dialogue.Resume();
            } else {
                dialogue.Pause();
            } 
        }
        if(key == Keys.Enter){
            dialogue.Hurry();
        }
    }
    void RenderText(){
        string content = dialogue.Text;

        var dialogueState = dialogue.State;
        if(dialogueState == DialogueState.Stopped){
            content += " <Stopped>";
        } else if (dialogueState == DialogueState.WaitingOk) {
            content += " <Ok>";
        } else if (dialogueState == DialogueState.Sleeping) {
            content += " <Sleep>";
        } else if (dialogueState == DialogueState.WaitingReply) {
            content += " <Reply>";
        }else if (dialogueState == DialogueState.Paused) {
            content += " <Paused>";
        }

        text.SetText(content);
    }

    private DialogueManager dialogue;
    private Nez.TextComponent text;
}
public class DialogueScene : Nez.Scene {
    public override void Initialize(){
        AddEntity(new DialogueEntity());
    }

}
