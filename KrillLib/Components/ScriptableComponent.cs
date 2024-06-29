using MoonSharp.Interpreter;

namespace Nez.Extension;


public class ScriptableComponent : Nez.Component {
    public ScriptableComponent(string scriptFilepath) : base(){
        _scriptFilepath = scriptFilepath;
    }
    
    private string _scriptFilepath;
}
