using Nez.Extension;
using MoonSharp.Interpreter;

public class D {
    [Fact]
    public void Unit1(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
    }
    [Fact]
    public void Unit2(){
        string content = File.ReadAllText("Assets/unit2.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(1, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit3(){
        string content = File.ReadAllText("Assets/unit3.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(2, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit4(){
        string content = File.ReadAllText("Assets/unit4.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(3, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit5(){
        string content = File.ReadAllText("Assets/unit5.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(4, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit6(){
        string content = File.ReadAllText("Assets/unit6.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(5, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit7(){
        string content = File.ReadAllText("Assets/unit7.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(6, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit8(){
        string content = File.ReadAllText("Assets/unit8.ldg");
        try {
            Dictionary <int, string> entries = LDG.FromString(content).Entries;
            Assert.True(false);
        } catch(LDGException e){
            Assert.Equal(7, e.ErrorCode);
        }
    }
    [Fact]
    public void Unit9(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
        
        Script script = new Script();
        DynValue function = script.DoString(entries[0]);
        DynValue output = script.Call(function);

        Assert.Equal(DataType.Number, output.Type);
        Assert.Equal(3, (int) output.Number);
    }
    [Fact]
    public void Unit10(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
        
        Script script = new Script();
        DynValue function = script.DoString(entries[1]);
        DynValue output = script.Call(function);

        Assert.Equal(DataType.Number, output.Type);
        Assert.Equal(7 + 11, (int) output.Number);
    }
    [Fact]
    public void Unit11(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
        
        Script script = new Script();
        script.Globals["global_value"] = 17;
        DynValue function = script.DoString(entries[2]);
        DynValue output = script.Call(function);

        Assert.Equal(DataType.Number, output.Type);
        Assert.Equal(17 + 4, (int) output.Number);
    }
    [Fact]
    public void Unit12(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
        
        Script script = new Script();
        DynValue function = script.DoString(entries[3]);
        DynValue output = script.Call(function);

        Assert.Equal(DataType.String, output.Type);
        Assert.Equal("Ahoy", output.String);
    }
    [Fact]
    public void Unit13(){
        string content = File.ReadAllText("Assets/unit1.ldg");
        Dictionary <int, string> entries = LDG.FromString(content).Entries;
        
        Script script = new Script();
        DynValue function = script.DoString(entries[4]);
        DynValue coroutine = script.CreateCoroutine(function);

        DynValue output = coroutine.Coroutine.Resume();

        Assert.Equal(DataType.Number, output.Type);
        Assert.Equal(3,(int) output.Number);

        output = coroutine.Coroutine.Resume();

        Assert.Equal(DataType.Number, output.Type);
        Assert.Equal(3.14,output.Number);

        output = coroutine.Coroutine.Resume();

        Assert.Equal(DataType.String, output.Type);
        Assert.Equal("end",output.String);

    }
}
