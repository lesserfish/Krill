using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Krill.Pipeline;

namespace Demo;

public class BasicScene : Nez.Scene {
    public BasicScene() : base() {}
    public override void Initialize() {
        LuaData lua = Content.Load<LuaData>("demo");
        System.Console.WriteLine(lua.Source);
    }
}

public class Game1 : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Scene = new BasicScene();
    }
}
