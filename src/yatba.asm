                processor 6502

                include "vcs.h"

                include "macro.h"

;--------------------------------------
NTSC            = 0
PAL             = 1
COMPILE_VERSION = PAL

V2K		= 2
V4K		= 4
COMPILE_SIZE	= V4K
;--------------------------------------

screenbyte	equ $80
pf1mirror	equ $81
tmp1		equ $82
pf3mirror	equ $83
tmp2		equ $84
tmp3		equ $85
seed		equ $86
tmp4		equ $87
p0ypos		equ $88
p0xpos		equ $89
p0yposold	equ $8A
p0xposold	equ $8B
timecnt		equ $8C
pgfxmir		equ $8D
mapmapidx	equ $8E
BGcolor		equ $8F

colmask		equ $90

screentile	equ $94
monmir		equ $96
playergfx	equ $98
playercol	equ $9A
mcmir		equ $9C
tilecolmir	equ $9E
 

tilecol		equ $A0
tilemap		equ $A8
monsterx 	equ $B0
mongfx 		equ $B8
healthbar	equ $C0
monlives	equ $C8
Score           equ $D0
Diam           equ $D1
DigitOnes       equ $D2
DigitTens       equ $D4
ScoreGfx        equ $D6
DiamGfx        equ $D7

roomidx		equ $E0
doroomupd	equ $E1
monstertype	equ $E2
canplayS1	equ $E5
songidx		equ $E4
fpspeed		equ $E6
nothing		equ $F0	;this is just an amazing life RNG!

;sprite reset timings
; ~ 29 34 39 44 50 55 60 65

;--------------------------------------
 
 IF COMPILE_SIZE = V4K		;if we have 4 kilobytes
                ORG $F000	; we start at $FFFF-4095
 ELSE						;else
                ORG $F800	; we start at $FFFF-2047
 ENDIF

 .byte "YatbaYdG"			;A little header for our game
Reset						;On reset
soft_reset
 cld						;clear the decimal flag (in case of)
 ldx #$00					
 ldy #$FF
clrmem
 lda $00,y
 eor $FF
 sta $FF
 stx $00,y
 dey
 bne clrmem

 ldx #$FF					;initialise the stack at $FF
 txs						;store it into the stack pointer
 lda $FF					;load the calculated start seed
 and #$7F					;make it <128
 sta seed					;and store it in the seed
 
main_code					;soft reset
;Now, we fill the healthbar
load_healthbar
 txa
 ldy #6					;the number of cases to fill
draw_healthbar_1
 dey					;decrease the index
 sta healthbar,y		;store it in the corresponding healthbar index
 bne draw_healthbar_1	;continue if not all filled
 lda #$35					;make start room $35
 sta mapmapidx


 IF COMPILE_SIZE = V4K		;if we make a 4k version
 lda #12					; then initialise the music register
 sta AUDC0
 ENDIF

 ;a lot of memory pointers are for the sprites. They are all at page $FF, so instead of writing each individual address, we fill the pointer memory at even offsets with $FF
 txa				;the value to fill with
fill_adr_loop
 sta screentile+1,y		;screentile is the first tile. Store it there+the y index
 iny					;increase the y index 2 times, to fill only the even bytes
 iny
 cpy #$C				;look if we are at the end
 bne fill_adr_loop		;if not, then continue

 lda #$10				;point to the default player index
 sta playergfx
 sta fpspeed
 lda #$18				;point to the default player color palette
 sta playercol
 lda #$99
 sta Diam


 lda #$3				;place the player in the room at X=3 Y=3
 sta p0ypos
 sta p0xpos




 ;lda #0					;make score 0 (probably redundant, to be removed)
 ;sta Score
 ;sta Diam

 ;lda #$C8				;load the default map tile type (probably redundant, to be removed)
 ;sta tilecolmir			;save it
 ;jmp VerticalBlank4		;and start the game (only needed to maintain the vblank when the reset is holded


restart_kernel
 ;if we compile for NTSC, we will draw the sunset. So we initialise the background to RED
 IF COMPILE_VERSION = NTSC
 lda #$20		;load the red color
 and colmask	;make it black&white if needed
 sta WSYNC		;wait for end of scanline (to prevent changing color in mid_scanline)
 sta COLUBK		;make it the background color
 sta WSYNC		;wait another time for the end of the scanline
 ENDIF

 sta WSYNC		;and wait a last time for the end of the scanline
position_player 
 nop			;the nops are here very important for the right positionning of the player
 nop
 lda.w p0xpos	;load the x position to set the player to
 and #$07		;verify if it isn't larger than 7
 cmp #4			;test if it is on the left or the right side
 bmi left_side_2;if it is on the left side, we run the code for the left side
 sbc #3			;substract 3, because we are on the right side
 sta tmp3		;
 ;;;;;;nop nop noperdepop;;;;;;;;;;;;;;
 dec nothing	;
 dec nothing	;
 dec nothing	;
 nop
 nop
 nop
 ;;;;;;end of the nop;;;;;;;;;;;;;;;;;;;
 ldy tmp3		;load the number of steps we need to wait

right_side_delay_2
 dey			;for each step, we wait 5 cycles.
 bne right_side_delay_2	

 sta RESP0		;now, at the right moment, reset the player pos
 jmp last_bar	;draw the sunset



left_side_2		;if we are on the left side
 tay			;load the number of steps we need to wait
left_side_delay_2
 dey			;for each step, we wait 5 cycles.
 bpl left_side_delay_2
 ;;;;;;nop nop noperdepop;;;;;;;;;;;;;;
 nop
 nop
 ;;;;;;end of the nop;;;;;;;;;;;;;;;;;;;
 sta RESP0		;and at the right moment, we reset the player pos

last_bar		;the sunset
 sta WSYNC		;wait for the end of the scanline
 ;we are only going to draw the sunset in NTSC mode, because in PAL mode we don't have enough place left
 IF COMPILE_VERSION = NTSC
 ldx #0			;load the mountain loading index
 lda #$20		;load the initial color
kersetloop
 clc
 adc #$11		;shift the color, and make it brighter
 inx			;increase the mountain index
 sta tmp1		;store the color temporairy
 and colmask	;make it black & white if needed
 sta WSYNC		;wait for the end of the scanline (to prevent changing color in mid_scanline)
 sta COLUBK		;store it as the color of the background
 and #$0F		;make it the index for the brightness of the mountains
 ora #$C0		;...and make it green
 and colmask	;...or black and white
 sta COLUPF		;...and store it in the mountains index
 lda tmp1		;reload our real color
 ldy bgmountain,x	;load the background
 sty PF0		;store it in the playfield registers
 sty PF1
 sty PF2		;...all of them (thanks to the strange mirroring)
 cmp #$CA		;look if we are at the end of the colors
 bne kersetloop	; else, we continue
;and if we are using PAL
 ELSE
 sta WSYNC		;we just wait a scanline
 ENDIF			;sorry, europeans...

 sta WSYNC		;waiting yet another scanline
 ldx #6			;this is going to be the number of tile rows
 lda BGcolor	;this is going to be the background color
 sta COLUBK		;so we save it as one
 lda #00		;we clear the playfield registers
 sta PF0
 sta PF1
 sta PF2		;...all of them



 jmp start_kernel	;...and we start the drawing of the main picture

some_rts
 rts
 
 ;confusion ahead... this is the code to position the monsters on the left side
left_side		;if we need to position the monsters on the left side
 tay			;load the number of steps we need to wait
left_side_delay
 dey			;for each step, we wait 5 cycles.
 bpl left_side_delay
 ;;;;;;nop nop noperdepop;;;;;;;;;;;;;;
 nop
 nop
 ;;;;;;end of the nop;;;;;;;;;;;;;;;;;;;
 sta RESP1		;and at the right moment, we reset the monster pos
 lda pf3mirror	;we load the right side of the playfield register 1
 nop
 nop
 nop
 sta PF1		;and we make it the right side
 jmp start_of_line_1	;and we continue the drawing of the playfield

start_kernel	;This is the real start of the kernel
 ldy #7			;load the number of lines per row
 sta WSYNC		;wait till the end of the line
main_kernel
 lda (monmir),y	;load the monster sprite
 sta GRP1
 
 lda pf3mirror		;load the right side of the playfield
 sta PF1

 lda (mcmir),y	;load the colors of the monsters
 and colmask	;make it black&white if needed
 sta COLUP1
 sty tmp4		;save temporary the y register (we need it to position the monster)
 
 sta WSYNC		;sync with end of scanline

 ;position the monsters (note part of this code is outside this block)
 lda pf1mirror		;load left side of the playfield
 sta PF1
 lda monsterx,x		;load the x position at which we have to position
 cmp #4				;look if it is on the left or the right side
 bmi left_side		;if it is at the left side, draw it there
 sbc #3				;else, we need 3 steps less
 sta tmp3			;save it temporary
 ;;;;;;nop nop noperdepop;;;;;;;;;;;;;;;
 dec nothing		
 dec nothing		
 dec nothing		
 ;;;;;;end of the nop;;;;;;;;;;;;;;;;;;;
 lda pf3mirror		;load the right side of the playfield
 sta PF1			
 ldy tmp3			;load the number of steps we need to delay

right_side_delay
 dey				;for each step, we wait 5 cycles.
 bne right_side_delay

 sta RESP1		;and position the monster at the right pos



start_of_line_1
 ldy tmp4		;we restore the y register
 sta WSYNC		;and wait 'till the end of the scanline

 IF COMPILE_VERSION = PAL	;if we are using PAL, we need to draw 1 extra dummy line, to have the right aspect ratio and no letterboxing
 lda pf1mirror	;load left side of the playfield
 sta PF1
 ;wait until left side drawn
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
 lda pf3mirror	;load right side of the playfield
 sta PF1
 sta WSYNC		;and wait for the end of the scanline
 ENDIF

 lda pf1mirror	;reload the left side of the playfield
 sta PF1

 lda tilemap,x		;load the current tilemap row
 sta screenbyte		; as the source

 lda (screentile),y	;load the drawing of tile?
 sta tmp1			;save it temporary for fast access
 
 ;load the representation of the right side
 asl screenbyte		;shift left the screen byte
 bcs dont_and_1		;if there isn't a tile there
 
 and #$0F			; then we erase the right (left? who knows with this stupid mirroring) side

dont_and_1
 asl screenbyte		;shift left the screen byte another time
 bcs dont_and_2		;if there isn't a tile there
 
 and #$F0			; then we erase the left side

dont_and_2
 sta tmp2			;weird... we first save it temporary
 lda pf3mirror		;we load our current right side
 sta PF1			; to continue our drawing with the ancient version
 lda tmp2			;then we reload our new version
 sta pf3mirror		; and make it the current version...
 
 ;load the representation of the center side
 lda tmp1			;load the drawing for the tile
 asl screenbyte		;shift left the screen byte
 bcs dont_and_3		;if there isn't a tile there

 and #$0F			; then we erase the right side

dont_and_3
 asl screenbyte		;shift left the screen byte yet another time
 bcs dont_and_4		;if there isn't now a tile there

 and #$F0			; then we erase the left side

dont_and_4
 lsr				;we shift it one bit right for our stupid playfield registers
 sta PF2			;and store it for the center. Note we are now more or less at the beginning of the second line, so we are safe to do that directly...
					; and of course, we don't need to mirror it!
 ;load the representation of the left side
 lda tmp1			;load the drawing for the tile
 asl screenbyte		;shift left the screen byte another time
 bcs dont_and_5		;if there isn't now a tile there

 and #$0F			; then we erase the right side (this is why i hate unrolling loops... you keep on repeating yourself!)

dont_and_5
 asl screenbyte		;shift left the screen byte a last time (note the last 2 bits are unused)
 bcs dont_and_6		;if there isn't some last tile there this time...

 and #$F0			; then we erase the left side

dont_and_6
 sta PF1			;finally, we are on the good position, so we draw it
 sta pf1mirror		; and store it as a mirror

 ;so... now, all our tile registers are loaded, so now, it is time for the players...
 
 cpx p0ypos			;look if we are at the right position for our gentle player
 beq draw_player	;if it is the case, we draw it
 lda #00			;else, we clear it
 .byte $2c			; and then do some kind of skip
draw_player
 lda (playergfx),y	; to prevent us from loading the player grafix
 sta tmp1			;and whatever we have, we store it temporary, to load the player quickly once the beam has passed by
 nop				;a little nop of timing fix...
 lda pf3mirror		; and we are ready to draw the right side of the playfield
 sta PF1		

 lda (playercol),y	;now, we load the color for the player
 and colmask		; make it black & white if needed
 sta tmp2			; and store it temporary, still waiting for the beam to pass by


 lda mongfx,x		;now, we load the pointer for the monster grafix
 sta monmir
 ora #8				;and the monster colors
 sta mcmir

 lda (tilecolmir),y	;finally, we load the color for the tiles
 and colmask		; black & white if needed
 sta COLUPF			;and finally, the beam has passed by, so we can store it savely
 ;note the color is one late on the tiles. this create that beautiful 3D-like effect... well, I happen to like it!
 lda tmp1			;we load our previously loaded grafix for the player
 sta GRP0
 lda pf1mirror		;we load the left side of the playfield
 sta PF1
 lda tmp2			; the color for the player
 sta COLUP0
 dey				;we look if we aren't at the end of the tile
 beq reset_y		; else we load a new tile
 jmp main_kernel	; and we restart


reset_y
 ldy #7				;reset Y to the tile's height
 dex				;decrease the row counter
 beq reset_x		;if we are at the end of the picture, then we start the bottom part
 jmp main_kernel	;else, we continue what we were doing

reset_x
 lda #0				;we clear the monster sprites
 sta GRP1
 ldy #3				;and we draw 3 dummy lines of playfield
 jmp bottom3
draw_bottom3
 lda pf1mirror		;+3
 sta PF1		;+3
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
 LDA ($80,X)
bottom3
 lda pf3mirror		;+3
 sta PF1		;+3
 sta WSYNC
 dey
 bne draw_bottom3
 ;note Y is now 0
 sty GRP0			;we clear the players grafix
 lda BGcolor		;we hide the background
 sta COLUPF
 lda healthbar+1	;and we start the loading of the healthbar
 sta PF1
 lda healthbar+2
 sta PF2


 lda #%00110000		;we put the playfield in repeating mode
 sta CTRLPF
 sta WSYNC			; wait till you known why
 lda #$02			;load a little grey
 IF COMPILE_VERSION = NTSC	;load the height of the healthbar
 ldx #6
 ELSE
 ldx #8
 ENDIF
 sta COLUPF			;and we make a littlish grey separator bar
 sta COLUBK 
 IF COMPILE_VERSION = NTSC	;we load the foreground color for the healthbar
 lda #$C8
 ELSE
 lda #$58
 ENDIF
 and colmask		;make it black & white if needed... you never know where those 2016'ers take their old TV from...
 tay				;for once, it will be the Y the temporary register
 IF COMPILE_VERSION = NTSC	;load the background color for the healthbar
 lda #$44			
 ELSE
 lda #$64
 ENDIF
 and colmask		;and black 'n white it
 sta WSYNC			;wait till end of scanline
 sty COLUPF			;store the front color
 sta COLUBK			; and the back one
vb4
 ;load left side of the healthbar
 lda healthbar
 sta PF0
 lda healthbar+1
 sta PF1
 lda healthbar+2
 sta PF2
 ;nopperdenop
 LDA ($80,X)
 LDA ($80,X)
 nop
 ;load the right one
 lda healthbar+3
 sta PF0
 lda healthbar+4
 sta PF1
 lda healthbar+5
 sta PF2
 ;and draw the number of lines needed
 dex
 sta WSYNC
 bne vb4

 ;the little grey bar...
 lda #$02
 sta COLUPF
 sta COLUBK 
 ;and a clear playfield...
 lda #0
 sta PF0
 sta PF1
 sta PF2
 sta WSYNC

 ;load the beige color for the other part of the footer
 IF COMPILE_VERSION = NTSC
 lda #$FA
 ELSE
 lda #$4A
 ENDIF
 and colmask	; or black & white, by the way...
 sta COLUBK
 clc
 sbc #6			;a little darker for the front...
 sta COLUPF

 ;and PAL wants another scanline... okay!
 IF COMPILE_VERSION = PAL
 sta WSYNC
 ENDIF

 ;a score routine for all your 4k'ers... note it is half-finished!
 IF COMPILE_SIZE = V4K
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Score display routine
;   source: http://www.randomterrain.com/atari-2600-lets-make-a-game-spiceware-03.html
PSFDskip     
   
 ldx #1          ; use X as the loop counter for PSFDloop
PSFDloop:
 lda Score,x     ; LoaD A with Diam(first pass) or Score(second pass)
 and #$0F        ; remove the tens digit
 sta tmp1        ; Store A into Temp
 asl             ; Accumulator Shift Left (# * 2)
 asl             ; Accumulator Shift Left (# * 4)
 adc tmp1        ; ADd with Carry value in Temp (# * 5)
 sta DigitOnes,x  ; STore A in DigitOnes+1(first pass) or DigitOnes(second pass)
 lda Score,x     ; LoaD A with Diam(first pass) or Score(second pass)
 and #$F0        ; remove the ones digit
 lsr             ; Logical Shift Right (# / 2)
 lsr             ; Logical Shift Right (# / 4)
 sta tmp1        ; Store A into Temp
 lsr             ; Logical Shift Right (# / 8)
 lsr             ; Logical Shift Right (# / 16)
 adc tmp1        ; ADd with Carry value in Temp ((# / 16) * 5)
 sta DigitTens,x ; STore A in DigitTens+1(first pass) or DigitTens(second pass)
 dex             ; DEcrement X by 1
 bpl PSFDloop    ; Branch PLus (positive) to PSFDloop
 
 ldx #5

ScoreLoop:              ;   43 - cycle after bpl ScoreLoop
 ldy DigitTens   ; 3 46 - get the tens digit offset for the Score
 lda DigitGfx,y  ; 5 51 -   use it to load the digit graphics
 and #$F0        ; 2 53 -   remove the graphics for the ones digit
 sta ScoreGfx    ; 3 56 -   and save it
 ldy DigitOnes   ; 3 59 - get the ones digit offset for the Score
 lda DigitGfx,y  ; 5 64 -   use it to load the digit graphics
 and #$0F        ; 2 66 -   remove the graphics for the tens digit
 ora ScoreGfx    ; 3 69 -   merge with the tens digit graphics
 sta ScoreGfx    ; 3 72 -   and save it
 sta WSYNC       ; 3 75 - wait for end of scanline
;---------------------------------------        
 sta PF1         ; 3  3 - @66-28, update playfield for Score dislay
 ldy DigitTens+1 ; 3  6 - get the left digit offset for the Diam
 lda DigitGfx,y  ; 5 11 -   use it to load the digit graphics
 and #$F0        ; 2 13 -   remove the graphics for the ones digit
 sta DiamGfx    ; 3 16 -   and save it
 ldy DigitOnes+1 ; 3 19 - get the ones digit offset for the Diam
 lda DigitGfx,y  ; 5 24 -   use it to load the digit graphics
 and #$0F        ; 2 26 -   remove the graphics for the tens digit
 ora DiamGfx    ; 3 29 -   merge with the tens digit graphics
 asl
 sta DiamGfx    ; 3 32 -   and save it
 jsr Sleep12     ;12 44 - waste some cycles
 sta PF1         ; 3 47 - @39-54, update playfield for Diam display
 ldy ScoreGfx    ; 3 50 - preload for next scanline 
 sta WSYNC       ; 3 53 - wait for end of scanline
;---------------------------------------
 sty PF1         ; 3  3 - @66-28, update playfield for the Score display
 inc DigitTens   ; 5  8 - advance for the next line of graphic data
 inc DigitTens+1 ; 5 13 - advance for the next line of graphic data
 inc DigitOnes   ; 5 18 - advance for the next line of graphic data
 inc DigitOnes+1 ; 5 23 - advance for the next line of graphic data
 jsr Sleep12     ;12 35 - waste some cycles
 IF COMPILE_VERSION = NTSC
 dex             ; 2 37 - decrease the loop counter
 ELSE
 nop
 ENDIF
 sta PF1         ; 3 40 - @39-54, update playfield for the Diam display
 IF COMPILE_VERSION = PAL	;PAL want's a 3-line kernel, where NTSC wants a 2-line one
 sta WSYNC
 sty PF1         ; 3  3 - @66-28, update playfield for the Score display
 jsr Sleep12
 nop
 nop
 nop
 nop
 jsr Sleep12     ;12 35 - waste some cycles
 dex             ; 2 37 - decrease the loop counter
 sta PF1         ; 3 40 - @39-54, update playfield for the Diam display
 
 ENDIF
 bne ScoreLoop   ; 2 42 - (3 43) if dex != 0 then branch to ScoreLoop
 sta WSYNC       ; 3 45 - wait for end of scanline
;---------------------------------------
 stx PF1         ; 3  3 - x = 0, so this blanks out playfield   
 
 ;goodness... I like when i don't have to code by myself! hihi...
 IF COMPILE_VERSION = PAL	;Add some extra lines for PAL
 sta WSYNC
 sta WSYNC
 sta WSYNC
 ENDIF
 ELSE	;for all you 2k folks
 
 ;we load the number of empty lines we need to wait
 IF COMPILE_VERSION = NTSC
 ldx #15
 ELSE
 ldx #20
 ENDIF
noscore_loop
 sta WSYNC	; and we wait...
 dex
 bne noscore_loop
 ENDIF    
 sta WSYNC	;last chance to wait!

 lda #%01000010	;and we put the playfield back in mirroring mode, for the picture to rebegin...
 sta VBLANK
 
 ;this time, no blankline guesswork anymode... we use our good old friend the timer to know if we are already ready!
 IF COMPILE_VERSION = NTSC
 lda #20
 ELSE
 lda #32
 ENDIF	; adjust to taste...
 sta TIM64T

 lda #%10000000	
 bit CXP0FB				;look if the player just landed in a rock
 bne random_pos_player	;if it is the case, time to look for somewhere else to land
 bit CXPPMM				;look if the player just landed in a monster
 bne hit_monster		;if it is the case, it's time to kill
 sta CXCLR				; and we clear all our old stuff

 lda timecnt			;load the volume for all our neath sound effects
 and canplayS1
 ;lsr
 sta AUDV1
 lda timecnt
 cmp #4					;look if we can stop the music
 bpl dont_stop_me_now
 lda #0					; if we can, then shut it
 sta AUDV0
 sta AUDV1
 sta AUDC0
 sta AUDC1
 sta AUDF1
 lda timecnt
dont_stop_me_now
 cmp #8					;look if we have to update the old position for the player
 bpl pass_decay_ppos
 lda p0ypos
 sta p0yposold
 lda p0xpos
 sta p0xposold
 
pass_decay_ppos

 dec timecnt		;decrease the time counter
 bmi set_timecnt_0_far	;zero it as needed
 jmp VerticalBlank2	;and update the monsters
eoupdate_player

 jmp VerticalBlank4	;else, we go directly to the next part
 
 ;the cake is a lie
Sleep12:            ;jsr here to sleep for 12 cycles        
        rts         ;ReTurn from Subroutine


random_pos_player	;auch... let's move from there
 ;lda #7				;but first... play some music!
 ;sta AUDC1			;zap!
 ;sta AUDF1			;zapperdeflap!
 ;lda #0				;don't know anymore what i'm doing here...
 ;sta mongfx,x
 ;lda nothing		;load some ugly dirty random number
 ;and #$7F			;not too big, if possible...
 ;sta mapmapidx		;and teleport to a screen there
 sta CXCLR			;clear the collision... omit this is you ain't a whimp!
 IF COMPILE_VERSION = NTSC	;wait a bit for a nice animation... epilepsy ahead!
 ;lda fpspeed
 ELSE
 ;lda fpspeed
 ENDIF
 ;sta timecnt
 jmp eoupdate_player	;and end
;------------------------------------------------
set_timecnt_0_far
 jmp set_timecnt_0	;my jump is to far away, so i need to do it in 2 steps...

hit_monster		;auch!
 lda #$FF
 sta canplayS1			;enable the sound
 ldx p0ypos				;look where we exactly hit the monster
 ;lda #$80
 ;cmp mongfx,x			;look if the monster is a potion... (this is nonsense... i implemented all kind of strange monsters, but forget the spawn code)
 ;beq drink_potion		;if it's the case, we drink it (the monster, not the potion ;-)
 ;lda #$90
 ;cmp mongfx,x			;look if the monster is a... whirlpool? (well, some volonteer to end my spawn code? can't decide how to do it!)
 ;beq random_pos_player	;then... zap!
 lda #$A0
 cmp mongfx,x			;look if the monster... is the finish? goodness...
 bne no_finish		;if it is not, do nothing
 lda #7				;but first... play some music!
 sta AUDC1			;zap!
 sta AUDF1			;zapperdeflap!
 jmp gameover_far		;year, the finish kill you... but don't tell it, please!
no_finish
 jsr decrease_lives		;okey... it's a real monster, so it really hurts!
 lda #8					;make some noise
 sta AUDC1
 sta AUDF1
 dec monlives,x			;and hurt mister monster too!
 bpl reset_pos_player	;finally, if possible, we jump back. Else... well poor you! poor monster too, by the way.)

 ;finally, we "kill" the "monster" (or potion, or whirlpool, or whatever it is...)
kill_monster
 lda #0			;load a zero
 sed			;set the 6502 in decimal mode
 sec			;just a cool way to say...
 adc Score		;...increase the score by 1
 sta Score		; and save the score
 cld			;and don't forget to put the machine back in binary mode... this kind of processor don't like to remain decimated!
 ;lda nothing	;i think this would be the code to load a potion
 ;beq load_potion
 lda #0
 ;.byte #$2c
load_potion
 ;lda #$80
 sta mongfx,x	;it doesn't work...

reset_pos_player	;reput us back to where we were, if possible!
 lda p0xposold		;(does i really need to explain this)
 sta p0xpos	
 lda p0yposold		;...okay... this code loads the old value of the position of the player
 sta p0ypos			; and put if back where it came from...
 sta CXCLR			;clear the collision (again, omit this if you ain't a whimp!)
 jmp eoupdate_player	;and and... ehh... end!

gameover_far
 jsr gameover			;it's a long road to home
 jmp reset_pos_player	;...when your branches are 1 byte long!

drink_potion
 ;lda #0				;"clear" the potion
 ;sta mongfx,x
 ;sta CXCLR			;...and the collision (omit this if you ARE a whimp!)
 ;lda #4				;make a glook glook sound (no? seriously?!)
 ;sta AUDC1
 ;sta AUDF1
 
 ;and increase the healthbar... the easy way!
 ;lda #$FF			
 ;ldx #6

search_next_empty	;search_next_empty...
 ;dex
 ;cmp healthbar,x
 ;bne search_next_empty
 ;...and fill it!
 ;inx
 ;sta healthbar,x
 ;ldx p0ypos
 ;jmp eoupdate_player

gameover_resetter	;i really have a bad memory... but it has something to do with the game over!
 lda INPT4
 bmi eoupdate_player_far
 jmp soft_reset
eoupdate_player_far
 jmp eoupdate_player

set_timecnt_0	;self-explaining, isn't it?
 lda #0
 sta timecnt

update_player
 lda songidx			; play the song
 IF COMPILE_SIZE = V4K	;if we have enough memory
 and #31				; which is 32 notes long
 tay
 lda song,y				;load the note
 sta AUDF0				;and make it beep as that
 lda #12				
 sta AUDC0
 lda #3				;load the volume
 sta AUDV0				; because me might have shut it down
 lda songidx
 and #%10100000
 beq no_beat
 lda songidx
 and #%11100000
 cmp #%11100000
 bne no_extro
 lda #00
 sta AUDV0
no_extro
 tya
 ENDIF
 and #7
 tay
 lda beat_type,y
 beq no_beat
 sta AUDC1
 lda beat_pitch,y
 sta AUDF1
 lda #5
no_beat
 inc songidx			;and increase the song index
 ;ELSE
 ;lda #0
 ;ENDIF
 sta canplayS1			;we silent the environnement
 ldy p0ypos				;load the player position
 sta CXCLR				;clear the collisions
 lda playergfx			;look if we are game over
 beq gameover_resetter
 lda #%10000000			;look if joystick is pushed right
 bit SWCHA
 beq moveright
 lsr					;look if joystick is pushed left (easier to do it this way)
 bit SWCHA		
 beq moveleft
 ldx p0xpos				;same thing, but this time for horizontal motion
 lsr					;look if joystick is pushed down
 bit SWCHA
 beq movedown
 lsr					;or finally, if up!
 bit SWCHA
 beq moveup
 ;nothing pressed, let's reset the time counter
 lda #0	
 sta timecnt
 ;IF COMPILE_SIZE = V4K	;and if we are in 4k
 sta AUDV0				;we silent the song
 sta AUDF0
 sta AUDC0
 dec songidx
 ;ENDIF
 jmp eoupdate_player	;and we are finished

moveleft
 lda #$10				;load the grafix for a player facing sidewards
 sta playergfx
 lda #%00000000			;mirror him the right way
 sta REFP0
 dec p0xpos				;decrease x pos
 bmi move_room_left		;if you move out of the screen, move to the next screen
 ldx p0xpos				;load the current x position
 jsr test_col_wall		;and look if there is a solid at that position
end_move_room_left
 bcs moveleft_cancel	;if there is, then we cancel the move...
 IF COMPILE_VERSION = NTSC	;else, we start the waiting (must not last to long...)
 ;lda #$0F
 ELSE
 ;lda #13
 ENDIF
 lda fpspeed
 sta timecnt
 jmp eoupdate_player	;and we are finished

sound_cancel
 lda #0					;no sound if we are in 4k
 IF COMPILE_SIZE = V4K
 sta AUDV0
 dec songidx
 ENDIF
 jmp eoupdate_player

moveleft_cancel			;oups, wrong move.
 inc p0xpos 			;just reverse our desicion
 jmp sound_cancel		;and we make no noise

moveright
 lda #%00001000   		;mirror him the right way
 sta REFP0
 lda #$10				;load the grafix for a player facing sidewards
 sta playergfx
 inc p0xpos				;increase x pos
 ldx p0xpos
 cpx #8					;look if we just moved out of the screen
 beq move_room_right	;if you move out of the screen, move to the next screen
 jsr test_col_wall		;look if we are in something hard and solid
 bcs moveright_cancel	;if it is the case... well, better step back
end_move_room_right
 IF COMPILE_VERSION = NTSC	;now we wait a little
 ;lda #$0F
 ELSE
 ;lda #13
 ENDIF					;...i said a little!
 lda fpspeed
 sta timecnt
 jmp eoupdate_player	; and we are finished

moveright_cancel		;oups...
 dec p0xpos				;just go back to our previous position
 jmp sound_cancel		;and cancel the noise

moveup
 lda #$28				;load the grafix for some player facing... up!
 sta playergfx
 inc p0ypos				;increase the vertical position... don't forget image is reversed!
 ldy p0ypos
 cpy #7					;look if we just moved out of the screen
 beq move_room_up		;if you move out of the screen, move to the next screen... thanks adventure for giving the idea
 jsr test_col_wall		;look if we did a stupid move...
 bcs moveup_cancel		;if it is the case, lets just step back
end_move_room_up
 IF COMPILE_VERSION = NTSC	;else, put a little time before the next step
 ;lda #$0F
 ELSE
 ;lda #13
 ENDIF
 lda fpspeed
 sta timecnt
 jmp eoupdate_player	;and we are ready!

moveup_cancel			;why do i keep on stepping in walls all the time?
 dec p0ypos				;if only it was so easy in RL...
 jmp sound_cancel		;...then we didn't make all that kind of noises

movedown
 lda #$20				;and now... the player looking down!
 sta playergfx
 dec p0ypos				;decrease to step down... there is no justice in an atari vcs
 ldy p0ypos	
 beq move_room_down		;look if we are gone out of the screen...
 jsr test_col_wall		;...or inside a wall
 bcs movedown_cancel	;it's time to start rerolling my loops!
end_move_room_down
 IF COMPILE_VERSION = NTSC	;bla bla bla
 ;lda #$0F	;bla bla bla bla bla
 ELSE
 ;lda #13	;bla bla bla bla bla
 ENDIF
 lda fpspeed
 sta timecnt	;bla bla bla
 jmp eoupdate_player	;and bla


move_room_left	;if we need to move the room left
 dec mapmapidx	;we decrease the room counter
 lda #7			;move the player...
 sta p0xpos		;...to the right of the screen
 sta p0xposold	;...and make sure it don't jump back in a wall if he touch a monster
 lda #$FF		;put a post-it to remain me to update the monsters
 sta doroomupd
 ;IF COMPILE_SIZE = V4K	;and if we are in 4k
 jsr decrease_rc		;decrease our room counter
 ;ENDIF
 jmp end_move_room_left	;end and

move_room_right	;same thing as above...
 inc mapmapidx	;...but replace left with right
 lda #0			;...and right with left
 sta p0xpos
 sta p0xposold
 lda #$FF
 sta doroomupd
 ;IF COMPILE_SIZE = V4K	;and if we are in 4k
 jsr decrease_rc		;decrease our room counter
 ;ENDIF
 jmp end_move_room_right
 
;what is it doing there? Oh! My branches are to far away!
movedown_cancel	;if we bumped in a wall moving down
 inc p0ypos		;then we move back
 jmp sound_cancel	;and don't make noise anymore


move_room_up	;moving rooms up is a little more complicated
 lda mapmapidx	;we load the map index
 clc
 sbc #$F		;substrace 16 from it
 and #$7F		;make sure it wraps around at the right time
 sta mapmapidx	;and save it
 lda #1			;then we define the new position for the player
 sta p0ypos
 sta p0yposold	;and the old one too...
 lda #$FF
 sta doroomupd	;remind to update the monsters
 ;IF COMPILE_SIZE = V4K	;and if we are in 4k
 jsr decrease_rc		;decrease our room counter
 ;ENDIF
 jmp end_move_room_up	;and we are ready!
 
move_room_down	;and moving rooms down is the same story
 lda mapmapidx
 clc
 adc #$10		;substraction instead of adding, for moving down instead of up
 and #$7F		;always making sure no wrap-around occurs
 sta mapmapidx	;and saving it the same way
 lda #6			;this time, we reposition the player on the upper-side of the screen
 sta p0ypos
 sta p0yposold	;and the old player too
 lda #$FF		;and finally
 sta doroomupd	;reminds us to update the monsters
 ;IF COMPILE_SIZE = V4K	;and if we are in 4k
 jsr decrease_rc		;decrease our room counter
 ;ENDIF
 jmp end_move_room_down	;and to move all down
 
 IF COMPILE_SIZE = 6	;=if we are in 4k
decrease_rc
 sed
 clc
 lda Diam
 sbc #0
 sta Diam
 bne decrease_rc_end	;if we took too much time
 sta playergfx			;time is up, so let's quit
 
decrease_rc_end
 cld
 clc
 rts
 ELSE
decrease_rc
 clc
 sed
 lda Diam
 sbc #0
 cld
 sta Diam
 beq game_over
 lsr
 lsr
 lsr
 lsr
 lsr
 clc
 IF COMPILE_VERSION = NTSC	;load the background color for the healthbar
 adc	#$C
 ELSE
 adc #8
 ENDIF
 sta fpspeed
 rts
game_over:
 sta playergfx			;time is up, so let's quit
 rts
 ENDIF

;------------------------------------------------
VerticalBlank2	;next stage of the vertical blank
 lda #$06		;look if it is time to update the monsters...
 cmp timecnt
 bne VerticalBlank4_prep
 lda #7			;for each monster
 sta tmp1
monloop
 jsr monupdate	;update the monster
 dec tmp1		;decrease the counter
 bne monloop	;and if we didn't update them all, continue
 jmp VerticalBlank4	;finally, go to the next stage of vertical blanking
;------------------------------------------------
monupdate
 ldx tmp1		;load the monster counter
 lda #$7F
 cmp mongfx,x	;look if the monster... is a monster!
 bmi end_mon_upd	;if not, end the update work
 lda monsterx,x
 sbc p0xpos		;look which side to move to
 beq end_mon_upd	;if we are on the same pos as the player, we don't have to do anything
 bcs mon_move_left	;elif we are on the right of the player, move left	(modify this if you are an ULTRAWHIMP)

mon_move_right		;if we need to move to the right,
 inc monsterx,x		;we increase the position of the respective monster
 txa
 tay				;move the monster index to the Y register
 ldx monsterx,y		;load the new position for the monster to the x register
 jsr test_col_wall	;test if there is something there
 bcs mon_move_right_cancel	;and if it is the case, move back
 rts				;else, we return

mon_move_left 		;if we need to move to the left
 dec monsterx,x		;we decrease the position of the respective monster
 txa				
 tay				;move the monster index to the Y register
 ldx monsterx,y		;load the new position for the monster to the x register
 jsr test_col_wall	;test if there is something there
 bcs mon_move_left_cancel	;and if it is the case, move back
 rts				;else, we return
 
mon_move_right_cancel
 ldx tmp1			;let's move the monster back
 dec monsterx,x		;we just decrease it
 rts				;and return

mon_move_left_cancel
 ldx tmp1			;same thing as above
 inc monsterx,x		;but with inc instead of dec
  ;this is wonderful with asm... you can reuse code for more than 1 routine!
end_mon_upd
 ldx tmp1			;reload a x pointer
 rts				;and return
;------------------------------------------------
montestcol	;what am i exactly trying to do here?
 ;lda #$30
 ;cmp mongfx,x
 ;beq montestcol_end
 ;sta monsterx,x
montestcol_end	;must be something pretty nice... i can't remember
 rts
;------------------------------------------------
VerticalBlank4_prep
	;historically, i had some code here, but the label is to be removed, and the referecnces to be replaced
VerticalBlank4
	;same thing here...
 ;jmp VerticalBlank5
;------------------------------------------------
VerticalBlank5
	;we are at the end of the first part of the vertical blank.
OSwait:		;so, we are going to wait until our timer halts to signal the time for the main vertical blank
 sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
 lda INTIM   ; Check the timer
 bpl OSwait  ; Branch if its Not Equal to 0

 lda #0		;clear the playfield registers
 ldx #2
 sta PF1
 sta PF0
 sta pf1mirror
 sta pf3mirror
 sta PF2		;... all of them
 stx VSYNC		;signal the start of a vertical sync

 sta WSYNC
 sta WSYNC
 sta WSYNC               ; we do 3 scanlines of VSYNC signal

 lda #0
 sta VSYNC      ;and signal the end of that same vertical sync

  ;now, we load the time to wait until the upper side of the picture is ready
 IF COMPILE_VERSION = NTSC
 lda #35	;good luck... both versions need to wait the same time!
 ELSE
 lda #35	;well, it isn't honest to omit the COMPILE_VERSION part...
 ENDIF
 
 sta TIM64T	;and put it in our friend the timer
     
;------------------------------------------------
; load the screen map
 ldy mapmapidx		;load the index of the map map

 lda mapmap,y		;load the index of the map
 ;now some magic to transform it into a tile type and a index
 clc
 lsr				;it's a kind of magic
 and #%00110000
 clc
 adc #$C0
 sta screentile
 adc #8				;well, this part finally isn't difficult to explain
 ;in fact, the color map is always 8 bytes away from the tile grafix
 ;so we add 8 to it
 sta tilecolmir		;and save it

 lsr	;now, load the type of tile
 lsr
 lsr	;it's 1/16th of the tile index
 lsr
 and #%00000011
 tax
 lda BGcolors,x	;and it's loaded from a mini-array with 4 values
 sta BGcolor
 
 lda #%00001000	;look if we are in black and white
 bit SWCHB
 bne dont_make_bw_back	;if not, no need to black_and_white it
 lda BGcolor		;else we make it bw
 and #%00001110
 sta BGcolor		
 lda #%00001110		;and we store a black and white mask for the rest of us
 sta colmask
 jmp end_make_bw
dont_make_bw_back
 lda #%11111110		;else we load just an empty mask... well, almost empty!
 sta colmask
 ;and so, we have the colors and tile types loaded...
end_make_bw
  ;this was the easy part...
  ;but it's now that the REAL weird things start to happen...
  ;we are going to transform a 4x8 bit value into a 6x6 value!
  ;...which is then transformed by the kernel into a 8x6 tilemap
  ;the only thing logical here is that a 0 is a clear tile, and a 1 a filled tile
  ;however, to explain the order of the bits is a different story...
 lda mapmap,y		;load the index for the grafix source
 asl				;remove the first 2 bits
 asl
 and #$7F			;make it point to the right place
 tax				;and make it an index
 lda mapmapgfx+0,x	;okay... load the first value
 asl				;remove the first 2 bits
 asl
 sta tilemap+1		;this is the first tile

 lda mapmapgfx+0,x	;load the first value again
 and #%11000000		; and the high part of it
 sta tilemap+3		; is going to serve somehow
 sta tilemap+4		; in the middle 2 tiles
 
 lda mapmapgfx+1,x	;load the second value
 asl				;remove the first 2 bits
 asl
 sta tilemap+2		;and store it as the second row

 lda mapmapgfx+1,x  ;load the second value again
 and #%11000000		;...well, just it's first 2 bits...
 lsr				;shift it right a bit
 lsr
 ora tilemap+3		;and it is only for the third row
 sta tilemap+3		;yaaahhh... i like that

 lda mapmapgfx+2,x	;load the third value
 asl				;shift it right like the others
 asl				
 sta tilemap+5		;and this is the 5th row

 lda mapmapgfx+2,x	;load the third value again
 and #%11000000		;...the first 2 bits...
 lsr				;shift them a bit (or 2)
 lsr
 ora tilemap+4		;and use them for the fourth row
 sta tilemap+4

 lda mapmapgfx+3,x	;now, load the last value
 asl				;impossible to reroll this loop
 asl
 sta tilemap+6		;the 6th row is defined

 lda mapmapgfx+3,x	;load that last value again
 and #%11000000		;and it
 lsr				;do some shifting
 lsr
 lsr
 lsr
 sta tmp1			;store it temporary
 ora tilemap+3		;use it for the 3rd row
 sta tilemap+3
 lda tmp1			;reload the value
 ora tilemap+4		;use it for the 4rd row too
 sta tilemap+4		;et voila!

 lda seed
 and #$7F
 sta seed
 cpy seed			;this must be my code for the seed
 bne dont_load_whirl	;but no...
 ;it doesn't seems to work
 ldx #3		;now, we place the whirl on the screen
 ldy #3
 jsr test_col_wall		;if there isn't already something there
 bcs randomnize_seed 	;else, we choise a new position
 stx monsterx+3			;else, we save it as if it was a monster
 lda #$A0
 sta mongfx+3			;and store its grafix too

dont_load_whirl
 tya					;use the room as a seed
 jsr randomnize			;well, more or less
 lda doroomupd			;look if we have to update the monster
 beq dont_load_monsters	;else, we beat it (ps: omit this if you are really sure you ain't a whimp)
 jsr load_monsters		;self-explaining
 lda SWCHB				;if the player IS a whimp
 bpl dont_load_monsters	;we are finished
 jsr load_monsters_hot	;else, do it again

dont_load_monsters
 lda #0					;notify ourself we did the monsters
 sta doroomupd
 lda #%00000001			;if the player doesn't want to restart the game
 bit SWCHB
 bne VerticalBlank		;rego to the main part. (invert this if you are a masochist)
 jmp soft_reset			;else reset the game

;------------------------------------------------
;a little routine to randominse the seed
randomnize_seed
 lda nothing		;load a random number
 sta seed			;and store it in the seed
 jmp VerticalBlank	;and then, restart the vertical blanking
;------------------------------------------------
 ;wait for timer 0
VerticalBlank
 sta WSYNC   ; Wait for HSYNC
 lda INTIM   ; Check the timer
 bpl VerticalBlank  ; Branch if its Not Equal to 0

 lda #00		;clear the vblank
 sta VBLANK
 lda #00		;blacks the background color
 sta COLUBK
 lda #%00110001	;invoque a mirroring playfield
 sta CTRLPF
 ;lda #%00000000	;and setup the player proprieties
 ;sta NUSIZ0
 ;sta NUSIZ1


 ldx #6			;load the number of rows
 ldy #7			; and the number of lines per row
 jmp restart_kernel	;and restart
 
end_of_load_monsters
 rts		;a little rts in the middle of nowhere... we all like that!

load_monsters
 lda #0		;first, we clear all current monsters
 ldx #6
clearmon_loop
 sta mongfx,x
 dex
 bne clearmon_loop

 lda mapmapidx	;load the room index as a seed
 eor seed		;well, almost. The current seed is a factor too
 sta tmp1		;finally, we store it temporary

load_monsters_hot	;jump here if you already have a seed
 lda screentile		;load type of room
 eor #$80			;xor of C0 (pos of room gfx) with 40 (pos of monster gfx)
 sta monstertype	;save it as the type of monster
 lda tmp1			;laod the seed
 jmp testter		;and jump in the middle of nowhere

loadmon_loop
 jsr lfsr			;randomnize the seed a little bit
 bcc end_of_load_monsters	;carried out half of the time
testter
 jsr randomnize		;randomnize the seed a lot
 tay
 and #%00000111	;load x pos
 sta tmp3
 tya
 jsr randomnize		;randomnize the seed a lot again
 sta tmp1
 and #%00000111	;load y pos
 sta tmp2
 tay
 cmp #2			;if the y position is larger than 6
 bmi loadmon_loop	;then retry
 ldx tmp3
 jsr test_col_wall	;if collisation with wall
 bcs loadmon_loop	;then retry
 ldy tmp2			;load the positions again
 ldx tmp3
 lda monstertype	;load the type of monster
 sta mongfx,y		;and save it
 stx monsterx,y		;store the x position
 lda tmp1			;reload the seed
 jsr randomnize		;randomnise
 tax
 and #%00000011		;make it a little lower
 sta monlives,y		;and save it
 txa
 jmp loadmon_loop	;and restart the loop

;---------------------------------------------
;this routine decrease the life by 1 if the player is hit
decrease_lives			;and it is a unrolled loop again... because of that stupid mirroring of stella's registers
 lsr healthbar+5		;decrease the first part of the healthbar
 bne end_decrease_lives	;íf it isn't empty, we are ready
 asl healthbar+4		;decrease the second part of the healthbar	
 bne end_decrease_lives	;íf it isn't empty, we are ready
 lsr healthbar+3		;ect...
 bne end_decrease_lives
 lsr healthbar+2
 bne end_decrease_lives
 asl healthbar+1
 bne end_decrease_lives
 lsr healthbar			;for the last healthbarpart, we decrease it
 lda healthbar
 cmp #$07				;look if it isn't completly hidden
 bne end_decrease_lives	
gameover				;and if there is only the hidden part full, the healthbar is considered empty, and the player wheens
 lda #00				;we effectivly says there is no player anymore
 sta playergfx			;by putting it's sprite on 0
end_decrease_lives
 rts					;and we return

;this little routine test if the instance is touching a wall
;Y=y position    X=x position   carry set=collision
test_col_wall
 lda tilemap,y					;we load the tilemap
 ldy test_col_wall_table,x		;we map it to a special table, which is going to traduct this all
test_col_wall_loop
 asl							;we shift it right the right number of times
 dey							;doen't modify the carry set by the shifting
 bne test_col_wall_loop			;and at the end, the carry will end at the right place
 rts

test_col_wall_table		;the number of times we need to shift
 ;       875612
 ;       874312
 .byte 5 ;1 -> 000010
 .byte 6 ;2 -> 000001
 .byte 4 ;3 -> 000100
 .byte 3 ;4 -> 001000
 .byte 3 ;5 -> 001000
 .byte 4 ;6 -> 000100
 .byte 2 ;7 -> 010000
 .byte 1 ;8 -> 100000
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; linear feedback shift register
;  this subroutine does a LFSR on the
;  accumulator
; input
;  A - the seed
; returns:
;  A - the result of the LFSR
;  carry - if the LFSR eor was performed
;   (random dependent on the accumulator)
lfsr
 lsr
 bcc noeor
 eor #$B4
 sec
noeor
 rts

randomnize
 ldx #9
randomnize_loop		;randomnize a bit
 jsr lfsr
 dex
 bne randomnize_loop
 rts

 
;--------------------------------------
BGcolors
;this are the colors for the background of the game
 IF COMPILE_VERSION = NTSC
 .byte $CF
 .byte $FF
 .byte $FF
 .byte $0F
 ELSE
 .byte $3F
 .byte $2F
 .byte $2F
 .byte $0F
 ENDIF
 
 IF COMPILE_SIZE = V4K
 ;this are, if needed, the digits for the score
DigitGfx
 .byte #%00000111
 .byte #%00000101
 .byte #%00000101
 .byte #%00000101
 .byte #%00000111

 .byte #%00010010
 .byte #%00010010
 .byte #%00010010
 .byte #%00010010
 .byte #%00010010

 .byte #%01110111
 .byte #%00010001
 .byte #%01110111
 .byte #%01000100
 .byte #%01110111

 .byte #%01110111
 .byte #%00010001
 .byte #%01110111
 .byte #%00010001
 .byte #%01110111

 .byte #%01010101
 .byte #%01010101
 .byte #%01110111
 .byte #%00010001
 .byte #%00010001

 .byte #%01110111
 .byte #%01000100
 .byte #%01110111
 .byte #%00010001
 .byte #%01110111

 .byte #%01110111
 .byte #%01000100
 .byte #%01110111
 .byte #%01010101
 .byte #%01110111

 .byte #%01110111
 .byte #%00010001
 .byte #%01110111
 .byte #%00010001
 .byte #%00010001

 .byte #%01110111
 .byte #%01010101
 .byte #%01110111
 .byte #%01010101
 .byte #%01110111

 .byte #%01110111
 .byte #%01010101
 .byte #%01110111
 .byte #%00010001
 .byte #%01110111
;this is the song. it is one from my own compositions... not a very good one, because i'm not a very good composer, but it will do the trick
song
 .byte #17
 .byte #17
 .byte #14
 .byte #15
 .byte #15 
 .byte #14
 .byte #17 
 .byte #15

 .byte #17
 .byte #17
 .byte #14
 .byte #15
 .byte #15 
 .byte #14
 .byte #17 
 .byte #17

 .byte #17
 .byte #17
 .byte #14
 .byte #15
 .byte #15 
 .byte #14
 .byte #17 
 .byte #15

 .byte #14
 .byte #14
 .byte #15
 .byte #15 
 .byte #16 
 .byte #16 
 .byte #17 
 .byte #17 
 ENDIF
 
beat_pitch
  .byte #30
  .byte #30
  .byte #0
  .byte #30
  .byte #0
  .byte #30
  .byte #0
  .byte #30
  
beat_type
  .byte #15
  .byte #15
  .byte #8
  .byte #15
  .byte #0
  .byte #15
  .byte #8
  .byte #15
;--------------------------------------
                ORG $FE00
mapmapgfx
 ;here are stored the different mapmapgrafix. I used a python script to generate them all... with this ordening, impossible to do it by hand
 ;the magic order as they end on screen is this:
 ; ___bit____1_____2____3__4____5_____6____7__8
 ; byte 1: 38&48 37&47  18 17 14&15 13&16 11 12  
 ; byte 2: 34&35 33&36  28 27 24&25 23&26 21 22 
 ; byte 3: 44&45 43&46  58 57 54&55 53&56 51 52  
 ; byte 4: 31&41 32&42  68 67 64&65 63&66 61 62  
 ;...where the numbers represent the position that bit will end on the screen
 
 ;I generated them with the extra/makemaptile.py tool.

       ;........;
       ;........;
       ;........;
       ;........;
       ;........;
       ;........;
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000

       ;###..###;
       ;##....##;
       ;...##...;
       ;..####..;
       ;#......#;
       ;###..###;
 .byte #%00110111
 .byte #%11100010
 .byte #%10110011
 .byte #%00110111

       ;###..###;
       ;##....##;
       ;#......#;
       ;#......#;
       ;##....##;
       ;###..###;
 .byte #%10110111
 .byte #%00110011
 .byte #%00110011
 .byte #%10110111

       ;########;
       ;#..##..#;
       ;........;
       ;........;
       ;#..##..#;
       ;########;
 .byte #%00111111
 .byte #%00101010
 .byte #%00101010
 .byte #%00111111

       ;########;
       ;##....##;
       ;#..##...;
       ;#.####..;
       ;#......#;
       ;###..###;
 .byte #%00110111
 .byte #%11100010
 .byte #%10110011
 .byte #%10111111

       ;########;
       ;#..##..#;
       ;......##;
       ;......##;
       ;##....##;
       ;###..###;
 .byte #%11110111
 .byte #%00110011
 .byte #%00101010
 .byte #%00111111

       ;###..###;
       ;##....##;
       ;..#..###;
       ;......##;
       ;###..###;
       ;########;
 .byte #%11111111
 .byte #%00110111
 .byte #%01110011
 .byte #%00110111

       ;###..###;
       ;#.....##;
       ;#.####..;
       ;#..##...;
       ;##....##;
       ;########;
 .byte #%00111111
 .byte #%10110011
 .byte #%11110010
 .byte #%10110111

       ;########;
       ;#..##..#;
       ;........;
       ;........;
       ;.......#;
       ;......##;
 .byte #%00110000
 .byte #%00100000
 .byte #%00101010
 .byte #%00111111

       ;###..###;
       ;##....##;
       ;##......;
       ;###..#..;
       ;###..#..;
       ;##......;
 .byte #%00000011
 .byte #%01000111
 .byte #%00110011
 .byte #%11110111

       ;##......;
       ;###..#..;
       ;..#..##.;
       ;......#.;
       ;###..###;
       ;########;
 .byte #%01111111
 .byte #%00110111
 .byte #%01000111
 .byte #%00000011

       ;......##;
       ;..#..###;
       ;.##..###;
       ;.#....##;
       ;##....##;
       ;###..###;
 .byte #%11110111
 .byte #%00110011
 .byte #%01110100
 .byte #%01110000

       ;########;
       ;##....##;
       ;...##...;
       ;........;
       ;#......#;
       ;###..###;
 .byte #%00110111
 .byte #%00100010
 .byte #%10110011
 .byte #%00111111

       ;###..###;
       ;##....##;
       ;......##;
       ;..#..###;
       ;###..###;
       ;###..###;
 .byte #%11110111
 .byte #%01110111
 .byte #%00110011
 .byte #%00110111

       ;###..###;
       ;#......#;
       ;........;
       ;..####..;
       ;########;
       ;########;
 .byte #%00111111
 .byte #%11111111
 .byte #%00100010
 .byte #%00110111

       ;###..###;
       ;#......#;
       ;#.####..;
       ;#..##...;
       ;##.....#;
       ;###..###;
 .byte #%00110111
 .byte #%10100011
 .byte #%11100010
 .byte #%10110111

       ;########;
       ;#..##..#;
       ;........;
       ;........;
       ;........;
       ;........;
 .byte #%00000000
 .byte #%00000000
 .byte #%00101010
 .byte #%00111111

       ;......##;
       ;.......#;
       ;.#.....#;
       ;.##..#.#;
       ;......##;
       ;......##;
 .byte #%10110000
 .byte #%01110000
 .byte #%00100000
 .byte #%01110000

       ;........;
       ;........;
       ;........;
       ;..#..#..;
       ;#..##..#;
       ;########;
 .byte #%00111111
 .byte #%01101010
 .byte #%00000000
 .byte #%00000000

       ;##......;
       ;#.......;
       ;#.......;
       ;#.#..#..;
       ;##......;
       ;##......;
 .byte #%00000011
 .byte #%01000011
 .byte #%00000010
 .byte #%10000011

       ;########;
       ;########;
       ;###..#..;
       ;##......;
       ;#.......;
       ;##......;
 .byte #%00000011
 .byte #%00000010
 .byte #%01111111
 .byte #%11111111

       ;########;
       ;###..###;
       ;......##;
       ;......##;
       ;..#..###;
       ;......##;
 .byte #%11110000
 .byte #%00110100
 .byte #%00110111
 .byte #%00111111

       ;......##;
       ;.......#;
       ;......##;
       ;......##;
       ;#..##..#;
       ;########;
 .byte #%11111111
 .byte #%00101010
 .byte #%00100000
 .byte #%00110000

       ;##......;
       ;##......;
       ;#.....#.;
       ;#..##.#.;
       ;########;
       ;########;
 .byte #%01111111
 .byte #%10111111
 .byte #%00000011
 .byte #%10000011

       ;##......;
       ;##......;
       ;.##..#..;
       ;.##..#..;
       ;........;
       ;........;
 .byte #%00000000
 .byte #%01000000
 .byte #%01000011
 .byte #%01000011

       ;......##;
       ;.......#;
       ;........;
       ;........;
       ;........;
       ;........;
 .byte #%00000000
 .byte #%00000000
 .byte #%00100000
 .byte #%00110000

       ;........;
       ;........;
       ;........;
       ;........;
       ;..#..###;
       ;......##;
 .byte #%00110000
 .byte #%00110100
 .byte #%00000000
 .byte #%00000000

       ;........;
       ;........;
       ;........;
       ;..#..#..;
       ;#.......;
       ;##......;
 .byte #%00000011
 .byte #%01000010
 .byte #%00000000
 .byte #%00000000

       ;###..###;
       ;###..###;
       ;.#....#.;
       ;.#....#.;
       ;........;
       ;........;
 .byte #%01000000
 .byte #%00000000
 .byte #%00110111
 .byte #%01110111

       ;......##;
       ;..#..#.#;
       ;........;
       ;........;
       ;..#..#.#;
       ;......##;
 .byte #%00110000
 .byte #%00100100
 .byte #%00100100
 .byte #%00110000

       ;........;
       ;........;
       ;...##...;
       ;..####..;
       ;#.....##;
       ;###..###;
 .byte #%00110111
 .byte #%11110010
 .byte #%10000000
 .byte #%00000000

       ;##......;
       ;#.......;
       ;......#.;
       ;..#..##.;
       ;###..#..;
       ;##......;
 .byte #%01000011
 .byte #%01000111
 .byte #%00000010
 .byte #%00000011
 ;of course, i made them, so don't expect wonders... but they are good enough for such a simple game

;--------------------------------------
mapmap
; here is stored the maptilemap
 .byte $14,$08,$0A,$12,$1B,$19,$08,$05,$54,$10,$30,$35,$27,$2D,$24,$2D
 .byte $17,$16,$64,$65,$17,$12,$0B,$02,$57,$1E,$1B,$39,$28,$2D,$2F,$26
 .byte $64,$63,$6D,$67,$63,$65,$07,$0D,$44,$0E,$1F,$3A,$36,$2F,$26,$64
 .byte $6E,$63,$6E,$65,$54,$7C,$08,$0D,$47,$05,$17,$36,$24,$2D,$64,$66
 .byte $74,$68,$63,$6D,$57,$1E,$16,$07,$43,$0E,$23,$25,$22,$29,$7C,$35
 .byte $77,$76,$64,$66,$54,$1C,$08,$03,$43,$23,$25,$2F,$2D,$37,$3E,$36
 .byte $34,$35,$67,$65,$57,$52,$56,$24,$25,$24,$21,$2D,$27,$2C,$2D,$24
 .byte $2A,$36,$54,$7C,$50,$48,$23,$26,$27,$2E,$2E,$2E,$25,$2F,$26,$22
 ;hand-drawn and hand-converted by me. Nothing fancy there...
;--------------------------------------
                ORG $FF00
;Here, finally, are stored the tiles... if you think they are ugly, well... it's up to you! I tried to do something good, but again, i'm not a professionnal pixel artist...
emptytile				;the representation of something not there...
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000
 .byte #%00000000
bgmountain			;note this tile is 10 height, so i put it in the half of the others. Please let it's first 2 bytes remain 0...
 .byte #%00000000
 .byte #%00000000

 .byte #%10000000
 .byte #%11000000
 .byte #%11100000
 .byte #%11110000
 .byte #%11111000
 .byte #%11111100
 .byte #%11111110
 .byte #%11111111
                ORG $FF10
playerside				;the side of the player
 .byte #%01100011
 .byte #%00110110
 .byte #%10011100
 .byte #%11111100
 .byte #%10011100
 .byte #%00101110
 .byte #%01111111
 .byte #%00111110

 IF COMPILE_VERSION = NTSC
 .byte #$00
 .byte #$02
 .byte #$08
 .byte #$06
 .byte #$08
 .byte #$FC
 .byte #$04
 .byte #$02
 ELSE
 .byte #$00
 .byte #$02
 .byte #$08
 .byte #$06
 .byte #$08
 .byte #$2C
 .byte #$04
 .byte #$02
 ENDIF
                ORG $FF20
playerfront				;the front of the player
 .byte #%01100011
 .byte #%00110110
 .byte #%10011100
 .byte #%11111111
 .byte #%10011100
 .byte #%00101010
 .byte #%01111111
 .byte #%00111110

playerback				;the back of the player	(the last 2 images use the same palette as the side of the player. Stop the wasting of bytes!)
 .byte #%11000110
 .byte #%01101100
 .byte #%00111001
 .byte #%11111111
 .byte #%00111001
 .byte #%01111100
 .byte #%11111110
 .byte #%01111100
                ORG $FF40
monster					;a first monster. It looks like an alien from space invaders...
 .byte #%00000000
 .byte #%01000010
 .byte #%00100100
 .byte #%10111101
 .byte #%01011010
 .byte #%00111100
 .byte #%01000010
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$40
 .byte #$46
 .byte #$44
 .byte #$42
 .byte #$40
 .byte #$42
 .byte #$44
 .byte #$42
 ELSE
 .byte #$60
 .byte #$66
 .byte #$64
 .byte #$62
 .byte #$60
 .byte #$62
 .byte #$64
 .byte #$62
 ENDIF

grolem				;A second monster... perfect for mountains!
 .byte #%00000000
 .byte #%00100100
 .byte #%10111101
 .byte #%11111111
 .byte #%11011011
 .byte #%01111110
 .byte #%00111100
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$F0
 .byte #$F6
 .byte #$F4
 .byte #$F2
 .byte #$F0
 .byte #$F2
 .byte #$F4
 .byte #$F2
 ELSE
 .byte #$20
 .byte #$26
 .byte #$24
 .byte #$22
 .byte #$20
 .byte #$22
 .byte #$24
 .byte #$22
 ENDIF

squirl				;A third monster... there are some tentacles out there!
 .byte #%00000000
 .byte #%01100110
 .byte #%00100100
 .byte #%11111111
 .byte #%01011010
 .byte #%01111110
 .byte #%00111100
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$A0
 .byte #$A6
 .byte #$A4
 .byte #$A2
 .byte #$A0
 .byte #$A2
 .byte #$A4
 .byte #$A2
 ELSE
 .byte #$90
 .byte #$96
 .byte #$94
 .byte #$92
 .byte #$90
 .byte #$92
 .byte #$94
 .byte #$92
 ENDIF

skeleton			;and the third one... seems hard to beat
 .byte #%00000000
 .byte #%11000011
 .byte #%10011001
 .byte #%00011000
 .byte #%01111110
 .byte #%10011001
 .byte #%01111110
 .byte #%00000000

 .byte #$00
 .byte #$06
 .byte #$04
 .byte #$02
 .byte #$00
 .byte #$02
 .byte #$04
 .byte #$02
                ORG $FF80
potion				;a potion
 .byte #%00000000
 .byte #%01111100
 .byte #%11111110
 .byte #%11011110
 .byte #%01111100
 .byte #%00010000
 .byte #%00111000
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$32
 .byte #$62
 .byte #$44
 .byte #$56
 .byte #$72
 .byte #$82
 .byte #$F4
 .byte #$F4
 ELSE
 .byte #$62
 .byte #$A4
 .byte #$64
 .byte #$86
 .byte #$C2
 .byte #$D2
 .byte #$44
 .byte #$44
 ENDIF
                ORG $FF90
whirl				;a... whirl?
 .byte #%00000000
 .byte #%01000000
 .byte #%10011100
 .byte #%10100011
 .byte #%10011001
 .byte #%01000010
 .byte #%00111100
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$D0
 .byte #$C2
 .byte #$B4
 .byte #$A6
 .byte #$78
 .byte #$6A
 .byte #$5C
 .byte #$4E
 ELSE
 .byte #$A0
 .byte #$82
 .byte #$64
 .byte #$46
 .byte #$36
 .byte #$54
 .byte #$72
 .byte #$90
 ENDIF
                ORG $FFA0
diamond				;this is still named diamond for historical reasons. It is what the player search
 .byte #%00000000
 .byte #%11000011
 .byte #%11011011
 .byte #%11011011
 .byte #%01111110
 .byte #%00111100
 .byte #%00011000
 .byte #%00000000

 .byte #$80
 .byte #$80
 .byte #$A2
 .byte #$96
 .byte #$BA
 .byte #$A8
 .byte #$A2
 .byte #$A2

                ORG $FFC0
forest					;the tile for the forest. A pretty cool pinetree
 .byte #%00000000
 .byte #%01000100
 .byte #%01000100
 .byte #%11101110
 .byte #%01000100
 .byte #%11101110
 .byte #%01000100
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$F4
 .byte #$F4
 .byte #$F4
 .byte #$C8
 .byte #$D2
 .byte #$C8
 .byte #$D2
 .byte #$D2
 ELSE
 .byte #$44
 .byte #$44
 .byte #$44
 .byte #$58
 .byte #$52
 .byte #$58
 .byte #$52
 .byte #$52
 ENDIF

mountains			;the tile for the mountains... a rock? a mountain? i dunna...
 .byte #%00000000
 .byte #%01000100
 .byte #%11101110
 .byte #%11101110
 .byte #%11101110
 .byte #%01000100
 .byte #%01000100
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$F0
 .byte #$F0
 .byte #$F0
 .byte #$F2
 .byte #$F2
 .byte #$F4
 .byte #$FB
 .byte #$FB
 ELSE
 .byte #$40
 .byte #$40
 .byte #$40
 .byte #$42
 .byte #$42
 .byte #$44
 .byte #$4B
 .byte #$4B
 ENDIF

sea					;this clearly is the sea... not to good drawn, but better than nothing
 .byte #%00000000
 .byte #%11001100
 .byte #%01100110
 .byte #%00110011
 .byte #%01100110
 .byte #%11001100
 .byte #%01100110
 .byte #%00000000

 IF COMPILE_VERSION = NTSC
 .byte #$8A
 .byte #$9A
 .byte #$AA
 .byte #$BA
 .byte #$AA
 .byte #$9A
 .byte #$8A
 .byte #$8A
 ELSE
 .byte #$7A
 .byte #$9A
 .byte #$BA
 .byte #$DA
 .byte #$BA
 .byte #$9A
 .byte #$7A
 .byte #$7A
 ENDIF
dungeon			;and this is for the dungeon... because of my st*pidity, it partly overlaps with the reset vector... which works well for PAL but not too good for NTSC
 .byte #%00000000
 .byte #%11101110
 .byte #%11101110
 .byte #%11101110
 .byte #%11101110
 .byte #%11101110
 .byte #%11101110
 .byte #%00000000

 .byte #$06
 .byte #$06
 .byte #$06
 .byte #$08


;--------------------------------------
                ORG $FFFC
 .word Reset          ; RESET
 .byte #$08				;and we don't have to define the IRQ/NMI vectors, so we use what arrange us the most... grey color!
 .byte #$08

 END
