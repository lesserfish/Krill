-- Interaction States
CLEAR=0
SAY=1
PAUSE=2
SLEEP=3
ASK=4
GOTO=5
CUSTOM=6
STOP=7

function Say(content)
    coroutine.yield(CLEAR)
    coroutine.yield(SAY, content)
end

function SayMore(content)
    coroutine.yield(SAY, content)
end

function Pause()
    coroutine.yield(PAUSE)
end

function Sleep(time)
    coroutine.yield(SLEEP, time)
end

function Ask(options)
    coroutine.yield(ASK, options)
    return __bridge_value
end

function Goto(code)
    coroutine.yield(GOTO, code)
end

function Stop()
    coroutine.yield(STOP)
end

function CustomMessage(...)
    local args = {...}
    coroutine.yield(CUSTOM, args)
    return __bridge_value
end

function SetTextSpeed(speed)
    __set_text_speed(speed)
end

function SetPauseAfterSay(bool)
    __set_pause_after_say(bool)
end

function SetInitialCode(code)
    __set_initial_code(code)
end

function SetSkipSpecial(bool)
    __set_skip_special(bool)
end
