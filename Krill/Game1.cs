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
        crontab.RegisterJob("l1", () => {System.Console.WriteLine("Hi (1)");}, "0 1/3 * *");
        crontab.RegisterJob("l2", () => {System.Console.WriteLine("Hi (2)");}, "0 2/3 * *");
        crontab.RegisterJob("l3", () => {System.Console.WriteLine("Hi (3)");}, "0 3/3 * *");
        victim.AddComponent(crontab);
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
    }

    protected override void Update(Microsoft.Xna.Framework.GameTime gameTime){
        base.Update(gameTime);
        Nez.Extension.Cron.Update();
    }
}

