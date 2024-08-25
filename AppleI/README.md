## Apple I


This is a rudimentary Apple I emulator. It runs both wozmon and Basic. Follow instructions below to get it running.

### Running

You need to specify

1. The BIOS binary
2. The BASIC binary
3. The FONT file

This emulator will look for these files in `Assets/` by default, so you can just clone this repository and run `cabal run` and it should work. If you want to specify custom locations, type `cabal run AppleI -- -h` to get a list of options.

Once you get it running, you will be faced with wozmon. Why don't you try running the demo program found in the [Apple I manual](https://github.com/lesserfish/Krill/blob/main/AppleI/Resources/Apple%20-%201%20operation%20manual%2C%201976%20-%20apple.applei.1976.102646518.pdf)

  1. Type `0: A9 0 AA 20 EF FF E8 8A 4C 2 0 (RET)`
  2. Type `0.A (RET)` to get the recently inputed bytes
  3. Type `R (RET)`.

You should see a stream of ASCII characters running on the screen. Press F1 to reset the Apple I.

Basic is automatically loaded into `E000`. To run it simply type

    E000 (RET)
    R (RET)

and you will be faced with the `>` Basic prompt. 

### Resources

The following resources were pretty handy:

1. [This stackexchange thread](https://retrocomputing.stackexchange.com/questions/13228/how-did-the-apple-1-video-circuit-work)
2. [This blog post about the shift register VRAM](http://www.righto.com/2022/04/inside-apple-1s-shift-register-memory.html)
3. [Another blog plot about the terminal](https://www.sbprojects.net/projects/apple1/terminal.php)
4. [This overview on how the Apple I works](https://www.youtube.com/watch?v=36NgkpctW6k)
5. [This incredibly entertaining video describing the history of the Apple I](https://www.youtube.com/watch?v=BHeUbAVllJo)
6. [This incredible explanation on the terminal](https://github.com/The8BitEnthusiast/apple-1-video-terminal-on-fpga)

### Todo

It would be cool to be able to read .wav cassettes and use the cassette interface at $C100. There seems to be very little documentation about cassette and I think I would rather start work on the Apple II. I will leave this as a future to-do.
