using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

namespace Demo;

public class BasicScene : Nez.Scene {
    public BasicScene() : base() {}
    public override void Initialize() {
    }
}

public class Game1 : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Scene = new BasicScene();
    }
}
