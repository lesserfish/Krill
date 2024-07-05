using var game = new Game();
game.Run();

public class Game : Nez.Core {
    protected override void Initialize(){
        base.Initialize();
        Scene = new Demo.DialogueScene();
    }
}
