using Nez.Extension;


public class FC {

    class ModifierController : FlowController {
        public ModifierController(Action modifier) {
            Modifier = modifier;
        }
        public override void Action(ControllableEntity? entity, Action next){
            Modifier();
            next();
        }

        public Action Modifier;
    }

    [Fact]
    public void Unit1(){
        int x = 0;
        Action modifier = () => {x = 3;};

        ModifierController controller = new ModifierController(modifier);
        controller.Action(null, () => {});

        Assert.Equal(3, x);
    }

    [Fact]
    public void Unit2(){
        int x = 1;
        Action modifier1 = () => {x += 3;};
        Action modifier2 = () => {x *= 2;};
        Action modifier3 = () => {x -= 1;};

        FlowController controller = FlowController.New()
                            .Join(new ModifierController(modifier1))
                            .Join(new ModifierController(modifier2))
                            .Join(new ModifierController(modifier3));

        controller.Action(null, () => {});

        Assert.Equal((1 + 3) * 2 - 1, x);

    }
}
