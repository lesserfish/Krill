namespace Krill;

public class SimpleScene : Nez.Scene 
{
    public override void Initialize(){
        base.Initialize();
        SetDesignResolution(1280, 720, SceneResolutionPolicy.None);
        Nez.Screen.SetSize(1280, 720);
        Nez.Console.DebugConsole.Instance.IsOpen = true;
        Nez.Console.DebugConsole.Instance.Log("Hello from SimpleScene!");
    }
    public override void Update(){
        base.Update();
    }

}

public class Engine : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Scene = new SimpleScene();
    }
}

