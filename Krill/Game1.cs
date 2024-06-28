namespace Krill;

public class SimpleScene : Nez.Scene 
{
    public override void Initialize(){
        base.Initialize();
        SetDesignResolution(1280, 720, SceneResolutionPolicy.None);
        Nez.Screen.SetSize(1280, 720);
        Nez.Console.DebugConsole.Instance.IsOpen = true;
        Nez.Console.DebugConsole.Instance.Log("Hello from SimpleScene!");
        Nez.Entity victim = CreateEntity("victim");
        Nez.Extension.CronTab crontab = new Nez.Extension.CronTab();
        victim.AddComponent(crontab).RegisterJob("l1", () => {Nez.Console.DebugConsole.Instance.Log("Hello every 5 seconds");}, "0 */5 * *");
    }
    public override void Update(){
        base.Update();
    }

}

public class Engine : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Nez.Extension.Cron.Instance = new Nez.Extension.Cron();
        Scene = new SimpleScene();
        Nez.Console.DebugConsole.ConsoleKey = Microsoft.Xna.Framework.Input.Keys.F12;
    }

    protected override void Update(Microsoft.Xna.Framework.GameTime gameTime){
        base.Update(gameTime);
        Nez.Extension.Cron.Update();
    }
}

