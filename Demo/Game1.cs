using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using EaseType = Nez.Tweens.EaseType;
namespace Demo;

public class BasicScene : Nez.Scene {
    public BasicScene() : base() {}
    public override void Initialize() {
        var player = new Nez.Extension.ClipPlayer();
        var entity = CreateEntity("entity");
        entity.AddComponent(player);
        var target = new Nez.Extension.PropertyTarget<int>(this, "val");
        player.Clip = Nez.Extension.ClipBuilder<int>.Start(target, 4)
                                .AddKey(3, 10, EaseType.Linear)
                                .AddKey(6, 12, EaseType.Linear)
                                .AddKey(8, 15, Nez.Tweens.EaseType.CubicIn)
                                .Finish()
                                .Start();

    }
    public override void Update(){
        base.Update();
        System.Console.WriteLine(val);
    }
    public int val = 4;
}

public class Game1 : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Scene = new BasicScene();
    }
}
