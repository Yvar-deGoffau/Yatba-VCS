# Yatba-VCS
A little game I programmed for the Atari 2600 in assembly in 2k. 

## Playing
 To play this game, you will need some form of Atari 2600 emulator (unless you have a real machine and a harmony cart). 
The binaries are in ./bin/

 This game is about some guy who is lost on a planet with a lot of monsters. They have already send him a space ship to rescue him, but he don't know when it is going to land, nor where it is going to land. Maybe the space ship has already landed? Maybe it is already gone?
 He need to find the space ship on time before the crew think he is lost forever. He hears the time tick in his head...
 But he has also to defend himself from the different monsters that this land is rich. And the land is so large, just finding the space ship is already going to be already a challenge! Has it landed in the forest, in the mountains, at the ruins of the ancient civilisation or near the endless ocean?

(ain't Atari games  famous for their storyline...) 

## Building
 You will need the DASM assembler to build the project. The VCS.h and Macro.h files are already included, but you might want to get the latest version of them. 

 You then need to modify the code at the top of the source file to build the right version:
    processor 6502

    include "vcs.h"

    include "macro.h"

    ;--------------------------------------

    NTSC            = 0

    PAL             = 1

    COMPILE_VERSION = PAL   ;Modify this line to "NTSC"/"PAL" to select your TV type

    V2K		= 2

    V4K		= 4

    COMPILE_SIZE    = V2K   ;Modify this line to "V2K"/"V4K" to select the 2K or the 4K version

    ;--------------------------------------

Then use the following command:
    `dasm.exe ./src/yatba.asm -s./src/yatba_version.sym -l./src/yatba_version.lst -f3 -o./bin/yatba_version.bin`
where you replace "version" with your compiled version.

 Normally, I already supply precompiled versions in the ./bin/ folder. They should normally be working on any emulator. I don't guarantee however that it will work on the real console; I don't have the tools needed to test it. Any photos of real consoles running this game are welcome!

## Technical Details
(maybe I'm going to put this in the wiki; but I don't have the time at the moment)
 You might wonder how it is possible that this game crams such a map in so little memory. In fact, while omitting the music and the timer, it fits perfectly in 2048 bytes of code, without using procedural generation, and that while I'm new at this platform and I didn't do much optimisations.
 The trick is that the game use a simple form of a tile-mapped storage & kernel.
 1 screen, represented as 8x6 tile images, is stored in 4 bytes. In fact, if you look well, you will see that there are some mirroring involved. With the extra/makemaptile.py tool (needs python and pygame!), you can experience by yourself how this mirroring works. The map is then stored as bits in 32 individual positions (4 bytesx8 bits), where a 1 represent a tile and a 0 represent no tile.
 There are 32 of these screens. In fact, you will often see the same screen, just with another texture!
 To determine which screen to load for the current position, the game looks-up in another table, 16x8 screens large. The lower 5 bits of 1 byte in this table represents the screen to load, the upper 2 bits gives the terrain type. This eats another 128 (16x8) bytes.
 This way, the game stores a 128x48 tilemap in just 256 bytes (not counting the code to load it). 
 There are other tricks involved in displaying this map (I'm really not using this hand-crafted kernel to it's max at the moment... maybe one day I will expand the 4K version to actually fits in 4096 bytes!) but I think most of them are piece of cake to seasoned Atari programmers. You will however see that because this game is turn-based, I did implement a custom-made sprite multiplexer without Flickering nor HMOVE bars!
 Finally, my code isn't the best; it repeats itself often, even outside the Kernel (look for the player-move code for an example; pure yuck!), and it is far from optimalised. Maybe someday I gonne fix the rough edges...

 P.S. I need to fix the Game aspect of the Game a little... for example the different monsters actually behave exactly the same! But I first need to shave some bytes off, I have none left for the 2k version.

 P.P.S: There is a lot of unused (i.e. commented) code out there... for when I have the time to finish it. What do you think of potions and warps?


##Licence
 At the moment I haven't decided yet... but fresh code is always welcome! By the way, I have no problems with getting cloned, reused code or anything the like; I won't get any money for it anyway :-P 
 As such, credits for reused code are fine, but not mandatory.
