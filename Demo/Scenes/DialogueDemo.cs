using Microsoft.Xna.Framework;
using Nez.Extension;
using System.IO;
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
        
    }
    public override void Update(){
        base.Update();
        string content = dialogue.Text;

        /*var dialogueState = dialogue.State;*/
        /*if(dialogueState == DialogueState.Stopped){*/
        /*    content += " <Stopped>";*/
        /*} else if (dialogueState == DialogueState.WaitingOk) {*/
        /*    content += " <Ok>";*/
        /*} else if (dialogueState == DialogueState.Sleeping) {*/
        /*    content += " <Sleep>";*/
        /*} else if (dialogueState == DialogueState.WaitingReply) {*/
        /*    content += " <Reply>";*/
        /*} else if (dialogueState == DialogueState.RunningLua) {*/
        /*    content += " <Lua>";*/
        /*}*/
        
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
