# Krill

Krill is just a bunch of 6502-based emulators written in Haskell.

I originally wrote a MOS 6502 emulator to power my [NES emulator](https://github.com/lesserfish/Shrimp) ( which I eventually ported to Rust). I didn't want to miss out on having a 6502 emulator written in Haskell so I've been messing around with 6502 based *stuff*.

So far, I've only finished emulating the Apple I. Apple II is currently in progress. Hopefully, I will be able to emulate the C64 as well.

## Apple I

Here is a screenshot of the Apple I. Go check it out [here](https://github.com/lesserfish/Krill/tree/main/AppleI)

![image](https://github.com/user-attachments/assets/df83f38a-c819-4817-922e-3baf95901262)


## Apple II

Currently in progress.


## Building

There is a flake.nix with all of the dependencies (for my homies in the Nix community). Simply run


    nix develop
    cabal update

And then just build whatever project you are interested in
