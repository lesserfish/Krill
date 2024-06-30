using Microsoft.Xna.Framework.Content.Pipeline;
using Microsoft.Xna.Framework.Content.Pipeline.Serialization.Compiler;
using System.IO;

namespace Krill.Pipeline;

[ContentImporter(".lua", DisplayName = "LuaImporter", DefaultProcessor = "LuaProcessor")]
public class LuaImporter : ContentImporter<string>
{
    public override string Import(string filename, ContentImporterContext context)
    {
        string source = File.ReadAllText(filename);
        return source;
    }
}

[ContentProcessor(DisplayName = "LuaProcessor")]
class LuaProcessor : ContentProcessor<string, LuaSource>
{
    public override LuaSource Process(string input, ContentProcessorContext context)
    {
        return new LuaSource(input);
    }
}

