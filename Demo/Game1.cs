using EaseType = Nez.Tweens.EaseType;
using Microsoft.Xna.Framework;
namespace Demo;


public class Other : Nez.Entity {
    public override void OnAddedToScene(){
        var player = new Nez.Extension.ClipPlayer();

        var clip = Nez.Extension.EventClipBuilder
                                .Start()
                                .AddKey(3, () => {System.Console.WriteLine("This is a simple action");})
                                .AddKey(6, this, "PrintX")
                                .AddKey(9, this, "IncreaseX", 5)
                                .AddKey(12, this, "PrintX")
                                .Loop(1)
                                .Finish()
                                .Start();
        
        player.Clip = clip;
        AddComponent(player);
    }
    public int X = 0;
    public void IncreaseX(int offset){
        X += offset;
    }
    public void PrintX(){
        System.Console.WriteLine("X = {0}", X);
    }
}

public class Triangle : Nez.Entity {
    public Triangle (EaseType t){
        type = t;
    }
    public override void OnAddedToScene(){
        var player = new Nez.Extension.ClipPlayer();
        var positionTarget = new Nez.Extension.PropertyTarget<Vector2>(this.Transform, "Position");
        var positionClip = Nez.Extension.ClipBuilder
                                .Start(positionTarget, new Vector2(100, 20))
                                .AddKey(3, new Vector2(800, 20), type)
                                .AddKey(6, new Vector2(800, 670), type)
                                .AddKey(9, new Vector2(100, 670), type)
                                .AddKey(12, new Vector2(100, 20), type)
                                .Finish();

        var rotationTarget = new Nez.Extension.PropertyTarget<float>(this.Transform, "Rotation");
        var rotationClip = Nez.Extension.ClipBuilder
                                .Start(rotationTarget, 0.0f)
                                .AddKey(6,3.14f, type)
                                .AddKey(12, 2 * 3.14f, type)
                                .Finish();

        player.Clip = new Nez.Extension.ClipBundle(positionClip, rotationClip).SetLoop(1).Start();
        AddComponent(player);
        AddComponent(new Nez.Sprites.SpriteRenderer(Scene.Content.LoadTexture("triangle")));
    }
    EaseType? type;
}


public class Square : Nez.Entity {
    public Square(EaseType? Type, float Y){
        type = Type;
        y = Y;
    }
    public override void OnAddedToScene(){
        var player = new Nez.Extension.ClipPlayer();
        var target = new Nez.Extension.PropertyTarget<Vector2>(this.Transform, "Position");
        player.Clip = Nez.Extension.ClipBuilder
                                .Start(target, new Vector2(100, y))
                                .AddKey(5, new Vector2(400, y), type)
                                .AddKey(10, new Vector2(800, y), type)
                                .Extend(12)
                                .Loop(1)
                                .Finish()
                                .Start();

        AddComponent(player);
        AddComponent(new Nez.Sprites.SpriteRenderer(Scene.Content.LoadTexture("square")));
    }

    private EaseType? type;
    private float y = 100;
}

public class BasicScene : Nez.Scene {
    public BasicScene() : base() {}
    public override void Initialize() {
        AddEntity(new Square(null,    1 * 50 + 20));
        AddEntity(new Square(EaseType.Linear,      2 * 50 + 20));
        AddEntity(new Square(EaseType.SineIn,      3 * 50 + 20));
        AddEntity(new Square(EaseType.SineOut,     4 * 50 + 20));
        AddEntity(new Square(EaseType.SineInOut,   5 * 50 + 20));
        AddEntity(new Square(EaseType.QuadIn,      6 * 50 + 20));
        AddEntity(new Square(EaseType.QuadOut,     7 * 50 + 20));
        AddEntity(new Square(EaseType.BounceIn,    8 * 50 + 20));
        AddEntity(new Square(EaseType.BounceOut,   9 * 50 + 20));
        AddEntity(new Square(EaseType.BounceInOut,10 * 50 + 20));
        AddEntity(new Square(EaseType.ElasticIn,  11 * 50 + 20));
        AddEntity(new Square(EaseType.ElasticOut, 12 * 50 + 20));

        AddEntity(new Triangle(EaseType.SineOut));

        AddEntity(new Other());

    }
}

public class Game1 : Nez.Core
{
    protected override void Initialize() {
        base.Initialize();
        Scene = new BasicScene();
    }
}
