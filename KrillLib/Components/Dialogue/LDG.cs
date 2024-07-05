using System.Collections.Generic;

namespace Nez.Extension;


public class LDGException : System.Exception {
    public LDGException(string message, string content, int errorCode) : base (message){
        Content = content;
        ErrorCode = errorCode;
    }

    public int ErrorCode;
    public string Content;

}
enum ParseState {
    ExpectingCode,
    Code,
    ExpectingSource,
    SourceContent,
}
public class LDG {
    
    public LDG(Dictionary<int, string> entries){
        Entries = entries;
        if(!entries.ContainsKey(0)){
            throw new LDGException("LDG needs to contain an entry of id 0.", "", 8);
        }
    }

    public static LDG FromString(string source){

        Dictionary<int, string> entries = new Dictionary<int, string>();

        string memory = "";

        int code = 0;
        int bracketCount = 0;
        string buffer = "";
        ParseState state = ParseState.ExpectingCode;

        foreach(char c in source){

            memory += c;
            if(state == ParseState.ExpectingCode){
                if(c == ' ' || c == '\n' || c == '\t'){
                    continue;
                }

                else if(c == '['){
                    state = ParseState.Code;
                    continue;
                }

                else {
                    throw new LDGException("Could not parse dialogue. Was expecting start of code block.", memory, 1);
                }
            }

            else if(state == ParseState.Code){
                if(c == ']'){
                    try {
                        code = int.Parse(buffer);
                        buffer = "";
                        state = ParseState.ExpectingSource;
                        continue;
                    } catch (System.Exception) {
                        throw new LDGException("Could not parse code. Was expecting an integer.", memory, 2);
                    }
                }
                else {
                    buffer += c;
                    continue;
                }
            }
            else if(state == ParseState.ExpectingSource){
                if(c == ' ' || c == '\n' || c == '\t'){
                    continue;
                }
                else if(c == '{'){
                    state = ParseState.SourceContent;
                    continue;
                }
                else {
                    throw new LDGException("Could not parse dialogue. Was expecting start of source block.", memory, 3);
                }
            }
            else if(state == ParseState.SourceContent){
                if(c == '{'){
                    buffer += c;
                    bracketCount++;
                    continue;
                }
                else if(c == '}' && bracketCount > 0){
                    buffer += c;
                    bracketCount--;
                    continue;
                }
                else if(c == '}' && bracketCount == 0){
                    if(entries.ContainsKey(code)){
                        throw new LDGException("Repeated code names in dialogue file.", memory, 4);
                    }

                    string parsedSource = "return function()\n" + buffer + "\nend";
                    entries.Add(code, parsedSource);

                    code = 0;
                    bracketCount = 0;
                    buffer = "";
                    state = ParseState.ExpectingCode;
                }
                else {
                    buffer += c;
                    continue;
                }
            }

        }

        if(state != ParseState.ExpectingCode){
            if(state == ParseState.ExpectingSource){
                throw new LDGException("Code block with no accompanying source block.", memory, 5);
            }
            else if(state == ParseState.Code){
                throw new LDGException("Unfinished code block.", memory, 6);
            }
            else if(state == ParseState.SourceContent){
                throw new LDGException("Unfinished source block.", memory, 7);
            }
        }

        return new LDG(entries);
    }

    public Dictionary<int, string> Entries;
}



