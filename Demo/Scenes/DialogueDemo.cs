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
                        if(Nez.Input.IsKeyPressed(Keys.D1))
                            HandleKeyDown(Keys.D1);
                        if(Nez.Input.IsKeyPressed(Keys.D2))
                            HandleKeyDown(Keys.D2);

                        RenderText();
                    }));
        
    }
    void HandleKeyDown(Keys key){
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
        if(key == Keys.D1){
            dialogue.Reply(1);
        }
        if(key == Keys.D2){
            dialogue.Reply(2);
        }
    }
    void RenderText(){
        DialogueState state = dialogue.State;
        if(state == DialogueState.WaitingReply){
            string content = dialogue.Text;
            foreach(var (entry, str) in dialogue.Options){
                content += "\n" + entry.ToString() + ") " + str;
            }
            text.SetText(content);
        } else {
            text.SetText(dialogue.Text);
        }
    }

    private DialogueManager dialogue;
    private Nez.TextComponent text;
}
public class DialogueScene : Nez.Scene {
    public override void Initialize(){
        AddEntity(new DialogueEntity());
    }

}
