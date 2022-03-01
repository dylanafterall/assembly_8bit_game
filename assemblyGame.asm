;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Dylan Campbell
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Initialize dasm, include files for register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        processor 6502		; code is for 6502 processor 
	include "vcs.h"		; standard defs of TIA and RIOT registers
   	include "macro.h"	; commonly used routines which aid in coding

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   	seg.u Variables		; defining segment for variables
   	org $80			; RAM starting point (origin)

PF1Ptr          word        	; pointer to PF1-value lookup table
PF2Ptr		word		; pointer to PF2-value lookup table

P0Damage	byte		; 2-digit damage counter stored as BCD
P1Damage	byte		; 2-digit damage counter stored as BCD
P0OnesOffset	word		; lookup table offset for P0's 1's digit
P0TensOffset	word		; lookup table offset for P0's 10's digit

P0XPos         	byte       	; player0 x-position
P0YPos         	byte        	; player0 y-position
P1XPos         	byte        	; player1 x-position
P1YPos         	byte        	; player1 y-position

P0tempXPos	byte		; used for collision - to return to previous
P0tempYPos	byte		; used for collision - to return to previous
P1tempXPos	byte		; used for collision - to return to previous
P1tempYPos	byte		; used for collision - to return to previous

P0SpritePtr    	word        	; pointer to player0 sprite lookup table
P0ColorPtr     	word        	; pointer to player0 color lookup table
P1SpritePtr    	word        	; pointer to player1 sprite lookup table
P1ColorPtr     	word        	; pointer to player1 color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SPRITE_HEIGHT   = 9      	; player sprite height constant
DIGIT_HEIGHT	= 5		; scoreboard digit height constant
ARENA_HEIGHT	= 80		; arena height constant

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   	seg Code		; defining segment for code
   	org $F000		; 2K ROM starts at $F800, 4K ROM at $F000

Reset:
   	CLEAN_START		; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   	lda #<P0Sprite
   	sta P0SpritePtr         ; lo-byte pointer for P0 sprite lookup table
   	lda #>P0Sprite
   	sta P0SpritePtr+1       ; hi-byte pointer for P0 sprite lookup table

   	lda #<P0Color
   	sta P0ColorPtr          ; lo-byte pointer for P0 color lookup table
   	lda #>P0Color
   	sta P0ColorPtr+1        ; hi-byte pointer for P0 color lookup table

   	lda #<P1Sprite
   	sta P1SpritePtr         ; lo-byte pointer for P1 sprite lookup table
   	lda #>P1Sprite
   	sta P1SpritePtr+1       ; hi-byte pointer for P1 sprite lookup table

   	lda #<P1Color
   	sta P1ColorPtr          ; lo-byte pointer for P1 color lookup table
   	lda #>P1Color
   	sta P1ColorPtr+1        ; hi-byte pointer for P1 color lookup table

   	lda #<PF1Data
   	sta PF1Ptr               ; lo-byte pointer for playfield lookup table
   	lda #>PF1Data
   	sta PF1Ptr+1             ; hi-byte pointer for playfield lookup table

	lda #<PF2Data
   	sta PF2Ptr               ; lo-byte pointer for playfield lookup table
   	lda #>PF2Data
   	sta PF2Ptr+1             ; hi-byte pointer for playfield lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #10
   	sta P0YPos              ; P0YPos = 10
   	lda #45
   	sta P0XPos              ; P0XPos = 60
   	lda #10
   	sta P1YPos              ; P1YPos = 10
   	lda #90
   	sta P1XPos              ; P1XPos = 100

	lda #$9A
    	sta COLUBK            	; set background(sky) color to blue
    	lda #$04
    	sta COLUPF            	; set playfield(platform) color to gray
    	lda #%00000001
    	sta CTRLPF            	; enable playfield reflection
        lda #$00
    	sta PF0               	; setting PF0 bit pattern
        
        lda #0
        sta P0Damage
        sta P1Damage		; P0, P1 start with no damage
        
      
      
      
      
      
      
      
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    	lda P0XPos
    	ldy #0
    	jsr SetObjectXPos      	; set player0 horizontal position

    	lda P1XPos
    	ldy #1
    	jsr SetObjectXPos      	; set player1 horizontal position
        
        jsr CalcDigitOffset	; calculate scoreboard digits table offset

    	sta WSYNC
    	sta HMOVE              	; apply the horizontal offsets previously set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Start the display loop: VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    	lda #2
    	sta VBLANK           	; turn on VBLANK
    	sta VSYNC            	; turn on VSYNC

    	; three lines of VSYNC
    	REPEAT 3
        	sta WSYNC     	; display 3 recommended lines of VSYNC
    	REPEND
    	lda #0
    	sta VSYNC            	; turn off VSYNC

    	; 37 lines of VBLANK
	REPEAT 37
        	sta WSYNC     	; display the 37 recommended lines of VBLANK
    	REPEND
    	sta VBLANK           	; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	KERNEL: 96 visible lines, double-height kernel (bottom 10 scoreboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    	ldx #86               	; X counts the number of remaining scanlines
.GameLineLoop:
	txa
        tay
	lda (PF1Ptr),Y
	sta PF1
	lda (PF2Ptr),Y
	sta PF2
.AreWeInsideJetSprite:
    	txa                   	; transfer X to A
    	sec                   	; make sure carry flag is set before subtraction
    	sbc P0YPos           	; subtract sprite Y-coordinate
    	cmp SPRITE_HEIGHT      	; are we inside the sprite height bounds?
    	bcc .DrawSpriteP0    	; if result < SpriteHeight, call the draw routine
    	lda #0                	; else, set lookup index to zero
.DrawSpriteP0:
    	tay                  	; load Y so we can work with the pointer
    	lda (P0SpritePtr),Y  	; load player0 bitmap data from lookup table
    	sta WSYNC            	; wait for scanline
    	sta GRP0              	; set graphics for player0
    	lda (P0ColorPtr),Y   	; load player color from lookup table
    	sta COLUP0            	; set color of player 0
.AreWeInsideBomberSprite:
    	txa                  	; transfer X to A
    	sec                 	; make sure carry flag is set before subtraction
    	sbc P1YPos           	; subtract sprite Y-coordinate
    	cmp SPRITE_HEIGHT     	; are we inside the sprite height bounds?
    	bcc .DrawSpriteP1    	; if result < SpriteHeight, call the draw routine
    	lda #0               	; else, set lookup index to zero
.DrawSpriteP1:
        tay                  	; load Y so we can work with the pointer
    	lda (P1SpritePtr),Y  	; load player1 bitmap data from lookup table
    	sta WSYNC             	; wait for scanline
    	sta GRP1             	; set graphics for player1
    	lda (P1ColorPtr),Y   	; load player color from lookup table
    	sta COLUP1            	; set color of player 1

    	dex                   	; X--
    	bne .GameLineLoop     	; repeat next main game scanline until finished

    	sta WSYNC             	; wait for final scanline
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda #0
        sta PF0
        sta PF1
        sta PF2
       	sta GRP0
        sta GRP1
        lda #$0F		; set playfield/scoreboard to white
        sta COLUPF
        lda #%00000000
        sta CTRLPF		; disable playfield reflection
        REPEAT 20		; 20 (not 10) due to single-line kernel
        	sta WSYNC
        REPEND
        
        ; reset settings for next frame 
        lda $04			; set playfield back to gray
        sta COLUPF
        lda #%00000001
    	sta CTRLPF            	; enable playfield reflection again

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Overscan:
	lda #2
    	sta VBLANK           	; turn on VBLANK again
    	REPEAT 30
        	sta WSYNC 	; display 30 recommended lines of VBlank Overscan
    	REPEND
    	lda #0
    	sta VBLANK           	; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
	lda #%10000000		; CXPPMM bit 7 detects P0 and P1 collision
        bit CXPPMM		; check CXPPMM bit 7 with the above pattern
        bne .CollisionP0P1	; collsion between P0, P1 happened
        jmp CheckCollisionP0PF	; skip to next check
.CollisionP0P1
        lda P0tempXPos
        sta P0XPos
        lda P0tempYPos
        sta P0YPos
        lda P1tempXPos
        sta P1XPos
        lda P1tempYPos
        sta P1YPos
        
CheckCollisionP0PF:
	lda #%10000000		; CXP0FB bit 7 detects P0 and PF collision
        bit CXP0FB		; check CSP0FB bit 7 with the above pattern
        bne .CollisionP0PF	; collision between P0 - PF happened
        jmp EndCollisionCheck	; skip to end of collision detection
.CollisionP0PF
	lda P0tempXPos
        sta P0XPos
        lda P0tempYPos
        sta P0YPos
        
CheckCollisionP1PF:
	lda #%10000000		; CXP0FB bit 7 detects P0 and PF collision
        bit CXP1FB		; check CSP1FB bit 7 with the above pattern
        bne .CollisionP1PF	; collision between P1  - PF happened
        jmp EndCollisionCheck	; skip to end of collision detection
.CollisionP1PF
	lda P1tempXPos
        sta P1XPos
        lda P1tempYPos
        sta P1YPos

EndCollisionCheck		; fallback
        sta CXCLR		; clear collision flags before next frame
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessJoystickP0:
        lda SWCHA       ; reads joystick positions
PJloopP0:    
        ldy P0XPos   	; save original X location so the player can be
        sty P0tempXPos 	;   bounced back upon colliding with the playfield
        ldy P0YPos   	; save original Y location so the player can be
        sty P0tempYPos  ;   bounced back upon colliding with the playfield
        asl             ; shift A bits left, R is now in the carry bit
        bcs CheckLeftP0	; branch if joystick is not held right
        ldy P0XPos   	; get the object's X position
        iny             ; and move it right
        cpy #144        ; test for edge of screen
        bne SaveXP0    	; save Y if we're not at the edge
	jsr GameOver	; ********************************************
SaveXP0:  
	sty P0XPos   	; saveX
        ldy #0      	; turn off reflect of player, which
        sty REFP0     	;   makes humanoid image face right

CheckLeftP0:
        asl             ; shift A bits left, L is now in the carry bit
        bcs CheckDownP0 ; branch if joystick not held left
        ldy P0XPos  	; get the object's X position
        dey             ; and move it left
        cpy #255  
        ; test for edge of screen
        bne SaveX2P0    ; save X if we're not at the edge
        ldy #159
	jsr GameOver	; ********************************************
SaveX2P0: 
	sty P0XPos	; save X
        ldy #8          ; turn on reflect of player, which
        sty REFP0     	;   makes humanoid image face left

CheckDownP0:
        asl                     ; shift A bits left, D is now in the carry bit
        bcs CheckUpP0           ; branch if joystick not held down
        ldy P0YPos              ; get the object's Y position
        dey                     ; move it down
        cpy #255                ; test for bottom of screen
        bne SaveYP0             ; save Y if we're not at the bottom
        jsr GameOver		; ********************************************
SaveYP0:  
	sty P0YPos              ; save Y

CheckUpP0:
        asl                     ; shift A bits left, U is now in the carry bit
        bcs EndJoystickP0       ; branch if joystick not held up
        ldy P0YPos              ; get the object's Y position
        iny                     ; move it up
        cpy #ARENA_HEIGHT       ; test for top of screen
        bne SaveY2P0            ; save Y if we're not at the top
        jsr GameOver		; ********************************************
SaveY2P0: 
	sty P0YPos              ; save Y
EndJoystickP0:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Process joystick input for player1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessJoystickP1:
        lda SWCHB       ; reads joystick positions
PJloopP1:    
        ldy P1XPos   	; save original X location so the player can be
        sty P1tempXPos 	;   bounced back upon colliding with the playfield
        ldy P1YPos   	; save original Y location so the player can be
        sty P1tempYPos 	;   bounced back upon colliding with the playfield
        asl             ; shift A bits left, R is now in the carry bit
        bcs CheckLeftP1 ; branch if joystick is not held right
        ldy P1XPos	; get the object's X position
        iny             ; and move it right
        cpy #144        ; test for edge of screen
        bne SaveXP1     ; save Y if we're not at the edge
	jsr GameOver	; ********************************************
SaveXP1:  
	sty P1XPos	; saveX
        ldy #8          ; turn off reflect of player, which
        sty REFP1     	; makes humanoid image face right

CheckLeftP1:
        asl             ; shift A bits left, L is now in the carry bit
        bcs CheckDownP1 ; branch if joystick not held left
        ldy P1XPos      ; get the object's X position
        dey             ; and move it left
        cpy #0       	; test for edge of screen
        bne SaveX2P1    ; save X if we're not at the edge
	jsr GameOver	; ********************************************
SaveX2P1: 
	sty P1XPos      ; save X
        ldy #0          ; turn on reflect of player, which
        sty REFP1	;   makes humanoid image face left

CheckDownP1:
        asl                     ; shift A bits left, D is now in the carry bit
        bcs CheckUpP1           ; branch if joystick not held down
        ldy P1YPos              ; get the object's Y position
        dey                     ; move it down
        cpy #255                ; test for bottom of screen
        bne SaveYP1             ; save Y if we're not at the bottom
	jsr GameOver		; ********************************************
SaveYP1:  
	sty P1YPos              ; save Y

CheckUpP1:
        asl                     ; shift A bits left, U is now in the carry bit
        bcs EndJoystickP1       ; branch if joystick not held up
        ldy P1YPos              ; get the object's Y position
        iny                     ; move it up
        cpy #ARENA_HEIGHT       ; test for top of screen
        bne SaveY2P1            ; save Y if we're not at the top
	jsr GameOver		; ********************************************
SaveY2P1: 
	sty P1YPos              ; save Y
EndJoystickP1:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    	jmp StartFrame        	; continue to display the next frame










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 	Horizontal Movement subroutine
;	A is the target x-coordinate position in pixels of our object
; 	Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    	sta WSYNC             	; start a fresh new scanline
    	sec                   	; make sure carry-flag is set before subtracion
.Div15Loop
    	sbc #15               	; subtract 15 from accumulator
	bcs .Div15Loop        	; loop until carry-flag is clear
    	eor #7                	; handle offset range from -8 to 7
    	asl
    	asl
    	asl
    	asl                   	; four shift lefts to get only the top 4 bits
    	sta HMP0,Y            	; store the fine offset to the correct HMxx
    	sta RESP0,Y           	; fix object position in 15-step increment
    	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Scoreboard Digits subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcDigitOffset subroutine
	

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
	lda #$30
        sta COLUBK
	rts










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	ROM Bitmaps - lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Playfield bitmaps (PF1 values, PF2 values)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	align $100            	; ensures start on page boundary (low byte $00)

;PlayfieldData:
; 	(# of lines),(PF1 value),(PF2 value)
;  	29,#%00000000,#%00000000	; sky
;  	4,#%00000000,#%11111111		; top platform
;  	20,#%00000000,#%00000000	; between floating platforms
;  	4,#%11111111,#%00000000		; two middle platforms
;  	20,#%00000000,#%00000000	; between ground, middle platforms
;  	9,#%11110000,#%11111111		; ground

PF1Data:
        .byte #0
        .byte #$0F		; line 86 (bottom of screen, last rendered)
        .byte #$0F		
        .byte #$0F
        .byte #$0F
        .byte #$0F
        .byte #$0F
        .byte #$0F		; 80
        .byte #$0F
        .byte #$0F
        .byte #$00		; 77 (end of segment five)
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 70
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 60
        .byte #$00
        .byte #$00
        .byte #$3F 		; 57 (end of segment four)
        .byte #$3F
        .byte #$3F		
        .byte #$3F
        .byte #$00		; 53 (end of segment three)
        .byte #$00
        .byte #$00
        .byte #$00		; 50
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 40
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00	
        .byte #$00
        .byte #$00		; 33 (end of segment two)
        .byte #$00
        .byte #$00
        .byte #$00		; 30 
        .byte #$00		; 29 (end of segment one)
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 20
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 10
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00        
        .byte #$00
        .byte #$00		; line 1 (top of screen, first rendered)

PF2Data:
	.byte #0
        .byte #$FF		; line 86 (bottom of screen, last rendered)
        .byte #$FF		
        .byte #$FF
        .byte #$FF
        .byte #$FF
        .byte #$FF
        .byte #$FF		; 80
        .byte #$FF
        .byte #$FF
        .byte #$00		; 77 (end of segment five)
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 70
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 60
        .byte #$00
        .byte #$00
        .byte #$03		; 57 (end of segment four)
        .byte #$03
        .byte #$03		
        .byte #$03
        .byte #$00		; 53 (end of segment three)
        .byte #$00
        .byte #$00
        .byte #$00		; 50
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 40
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$FC		; 33 (end of segment two)
        .byte #$FC
        .byte #$FC
        .byte #$FC		; 30
        .byte #$00		; 29 (end of segment one)
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 20
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; 10
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00
        .byte #$00		; line 1 (top of screen, first rendered)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P0 Bitmap - Stationary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Sprite:
    	.byte #%00000000        ;
    	.byte #%01101100        ;
    	.byte #%00111000        ;   xxx
    	.byte #%01111100        ;  xxxxx
    	.byte #%11111110        ; xx x xx
    	.byte #%11010110        ; xxxxxxx
    	.byte #%01111100        ;  xxxxx
    	.byte #%00111000        ;   xxx
    	.byte #%00000000        ;  xx xx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P0 Bitmap - Running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0SpriteRun:
    	.byte #%00000000        ;
    	.byte #%00110110        ;
    	.byte #%00011100        ;    xxx
    	.byte #%00111110        ;   xxxxx
    	.byte #%01111111        ;  xxx x x
    	.byte #%01110101        ;  xxxxxxx
    	.byte #%00111110        ;   xxxxx
    	.byte #%00011100        ;    xxx
    	.byte #%00000000        ;   xx xx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P1 Bitmap - Stationary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P1Sprite:
    	.byte #%00000000
    	.byte #%01011100        ; x  xx
    	.byte #%01111110        ; x  x
    	.byte #%01111011        ; xxxx
    	.byte #%11110011        ; x x   xx
    	.byte #%10100011        ; xxxx  xx
    	.byte #%11110000        ;  xxxx xx
    	.byte #%10010000        ;  xxxxxx
    	.byte #%10011000        ;  x xxx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P1 Bitmap - Running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P1SpriteRun:
    	.byte #%00000000
    	.byte #%10001100        ; xx xx
    	.byte #%11111100        ; x  x
    	.byte #%01111110        ; xxxx
    	.byte #%11110011        ; x x   xx
    	.byte #%10100011        ; xxxx  xx
    	.byte #%11110000        ;  xxxxxx
    	.byte #%10010000        ; xxxxxx
    	.byte #%11011000        ; x   xx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P0 Bitmap - Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    	.byte #$00
    	.byte #$42
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E

P0ColorRun:
    	.byte #$00
    	.byte #$42
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E
    	.byte #$5E

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	P1 Bitmap - Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P1Color:
    	.byte #$00
    	.byte #$1E
    	.byte #$1E
    	.byte #$1E
    	.byte #$1E
    	.byte #$00
    	.byte #$1E
    	.byte #$1E
    	.byte #$00

P1ColorRun:
    	.byte #$00
    	.byte #$1E
    	.byte #$1E
    	.byte #$1E
    	.byte #$1E
    	.byte #$00
    	.byte #$1E
    	.byte #$1E
    	.byte #$00

Digits:
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
	
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
	
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00110011          ;  ##  ##
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###

    	.byte %01010101          ; # # # #
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01000100          ; #   #
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #
    	.byte %00010001          ;   #   #

    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###

    	.byte %01110111          ; ### ###
    	.byte %01010101          ; # # # #
    	.byte %01110111          ; ### ###
    	.byte %00010001          ;   #   #
    	.byte %01110111          ; ### ###










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   	org $FFFC                ; move to position $FFFC
   	word Reset
   	word Reset
