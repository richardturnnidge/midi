;	Serial MIDI input demo
; 	Richard Turnnidge 2024

; 	Reads MIDI serial data from keyboard and plays sounds


; ---------------------------------------------
;
;	CONSTANTS
;
; ---------------------------------------------

midi_ID:	equ	144 	; Keystation keyboard ID

; ---------------------------------------------
;
;	MACROS
;
; ---------------------------------------------

	macro MOSCALL afunc
	ld a, afunc
	rst.lil $08
	endmacro

; ---------------------------------------------
;
;	INITIALISE
;
; ---------------------------------------------

	.assume adl=1						; big memory mode
	.org $40000						; load code here

	jp start_here						; jump to start of code

	.align 64						; MOS header
	.db "MOS",0,1

	;include "debug_routines.asm"

; ---------------------------------------------
;
;	INITIAL SETUP CODE HERE
;
; ---------------------------------------------

start_here:
								; store everything as good practice	
	push af							; pop back when we return from code later
	push bc
	push de
	push ix
	push iy


	call CLS 						; clear screen
	call openUART1						; init the UART1 serial port
	call hidecursor						; hide the cursor

	ld hl, text_data
	ld bc, end_text_data - text_data
	rst.lil $18						; print default text to screen

	call loadSample

; ---------------------------------------------
;
;	MAIN LOOP
;
; ---------------------------------------------

MAIN_LOOP:	
	MOSCALL $08								; get IX pointer to sysvars
	ld a, (ix + 05h)						; ix+5h is 'last key pressed'
	cp 27									; is it ESC key?
	jp z, exit_here							; if so exit cleanly

	call uart1_handler						; get any new data from UART1

	ld a, (uart1_received)					; check if we got anything
	cp 0
	jr z, MAIN_LOOP							; nothing new, loop round again

	ld a, (uart1_received)					; grab latest data				

	call manageData							; deal with data

	jp MAIN_LOOP


is_playing:		.db 0						; quick hack for single not playing
waitingNote:	.db 1						; if waiting for note to arrive
waitingValue:	.db 0						; if waiting for hit value to arrive

note:			.db 0
value:			.db 0

; ---------------------------------------------
; deal with incoming data

manageData:
	ld a, (uart1_received)					; grab latest data	
	cp midi_ID								; is is a signal from keyboard?
	jr z, keyboardSignal					; is keyboard id

	ld a, (waitingNote)
	cp 1
	jr z, getNote							; get the note	
											; else we must get Value

getValue:

	ld a, (uart1_received)					; this is our Value
	ld (value), a  							; we can now start the note

	call startSound

	ret


getNote:

	ld a, (uart1_received)					; this is our Note
	ld (note), a

	ld a, (is_playing)
	cp 1

	jp z, stopSound							; if playing, we need to stop

	ld a, 0
	ld (waitingNote), a  					; we have note, now need value
	ld a, 1
	ld (waitingValue), a

	ret

keyboardSignal:

	ld a, 1
	ld (waitingNote), a 					; now wait for a note

	ret


startSound:

	ld a, 0
	ld (waitingNote), a  					; reset waitingNote
	ld (waitingValue), a  					; reset waitingValue

	ld a, 1
	ld (is_playing), a

	; set the required note quick hack to test

	ld a, (note)						; this is midi number
	or a
	sla a	
	and 11111110b						; x2

	push af 


	ld b, 7
	ld c, 10
	call setTAB
	ld hl, blanks
	call printString

	ld b, 7
	ld c, 10
	call setTAB
	pop af 
	push af
	call printDec

	pop af	

	; A is offset

	ld HL, midiFrequencies
	ld de, 0						; reset to 0
	ld e, a 						; the offset
	add hl, de 						; get offset into data

	ld a, (hl)
	ld (theNote), a  					; set LSB

	inc hl
	ld a, (hl)
	ld (theNote + 1), a  					; set MSB

	ld hl, playNote
	ld bc, endPlayNote - playNote
	rst.lil $18

	; print it out
	ld b, 7
	ld c, 2

	call setTAB
	ld a, (note)
	call printDec

	ld b, 7
	ld c, 4
	call setTAB
	ld a, (value)
	call printDec

	ld b, 7
	ld c, 6
	ld a, (theNote)
	call debugA

	ld b, 10
	ld c, 6
	ld a, (theNote + 1)
	call debugA


	ld b, 7
	ld c, 8

	call setTAB

	ld hl, blanks
	call printString

	ld b, 7
	ld c, 8

	call setTAB

	ld a, (theNote)
	call printDec

	ld b, 11
	ld c, 8

	call setTAB
	ld a, (theNote + 1)
	call printDec


	ret

blanks:		.blkb 10,32
		.db 0
stopSound:

	ld a, 0
	ld (waitingNote), a  					; reset waitingNote
	ld (waitingValue), a  					; reset waitingValue
	ld (is_playing), a

	ld hl, stopNote
	ld bc, endStopNote - stopNote
	rst.lil $18

	ret 



playNote:

		.db 23, 0, $85, 0, 0, 127			; play note
theNote:	.dw $0
theLength:	.dw -1

endPlayNote:

stopNote:

	.db 23, 0, $85, 0, 12
	.dw 0									;  channel 0 duration
	.db 0



endStopNote:

; ------------------

loadSample:                             	; for loading audio samples

	ld hl, sampleStr                    	; start of data to send
	ld bc, endSampleStr - sampleStr     	; length of data to send
	rst.lil $18                         	; send data
	ret 

sampleStr:
	.db     23,0,85h                    	; audio command
	.db     -1, 5                       	; sample number, sample management
	.db     0                           	; load
	.dl     18171                       	; length in bytes (LONG)
	incbin "violin.raw"


	.db     23,0,85h                    	; audio command
	.db     -1, 5                       	; sample number, sample management
	.db     3                           	; set baseline freq
	.dw     264								; the base freq


	.db 23, 0, $85, 0, 4					; set ch0 to sample mode
	.db -1									; set which sample


	.db 23, 0, $85, 0, 6, 1					; set ch0 envelope
	.dw 80									; Attack
	.dw 120									; Decay
	.db 63									; Sustain
	.dw 1600								; Release



endSampleStr:



; ---------------------------------------------
; Adapted from Hexload source

uart1_handler:		
	DI
	PUSH	AF
	IN0	A,(REG_LSR)				; Get the line status register
	AND	UART_LSR_RDY				; Check for characters in buffer
	JR	Z, noData 				; Nothing received
			
	LD	A,1   					; we got new data
	LD	(uart1_received),a 			; so set flag fro new data
	IN0	A,(REG_RBR)				; Read the character from the UART receive buffer
	LD	(uart1_buffer),A  			; store new byte of data
	POP	AF
	EI

 	ld a, (uart1_buffer)				; check it is a control code. Exit if not
	LD (uart1_received), A				; store it

	RET

noData:
	XOR 	A,A
	LD	(uart1_received),A			; note that nothing is available	
	POP	AF
	EI

	RET
			
uart1_buffer:		.db	1			; 64 byte receive buffer
uart1_received:		.db	1			; boolean
uart1_data:		.db	1			; final pot value received
uart1_command: 		.db 	0			; current command received

; ---------------------------------------------
;
;	UART CODE
;
; ---------------------------------------------

openUART1:
	ld ix, UART1_Struct
	MOSCALL $15					; open uart1
	ret 

; ---------------------------------------------

closeUART1:
	MOSCALL $16 					; close uart1
	ret 

; ---------------------------------------------

UART1_Struct:	
	.dl 	115200; 9600					; baud (stored as three byte LONG)
	.db 	8 					; data bits
	.db 	1 					; stop bits
	.db 	0 					; parity bits
	.db 	0					; flow control
	.db 	0					; interrupt bits

; ---------------------------------------------
;
;	EXIT CODE CLEANLY
;
; ---------------------------------------------

exit_here:

	call closeUART1
	call CLS
							; reset all values before returning to MOS
	pop iy
	pop ix
	pop de
	pop bc
	pop af
	ld hl,0

	ret						; return to MOS here

; ---------------------------------------------
;
;	OTHER ROUTINES	
;
; ---------------------------------------------

CLS:
	ld a, 12
	rst.lil $10					; CLS
	ret 

; ---------------------------------------------

hidecursor:
	push af
	ld a, 23
	rst.lil $10
	ld a, 1
	rst.lil $10
	ld a,0
	rst.lil $10	;VDU 23,1,0
	pop af
	ret


; ---------------------------------------------
; strings used

LINEFEED:           .asciz "\r\n"

printString:                ; print zero terminated string
    ld a,(hl)
    or a
    ret z
    RST.LIL 10h
    inc hl
    jr printString


; ---------------------------------------------
;
;	DEBUG ROUTINES
;
; ---------------------------------------------
	
; ---------------------------------
; print decimal value of 0 -> 255 to screen at current TAB position

printDec:               ; debug A to screen as 3 char string pos

    ld (base),a         ; save

    cp 200              ; are we under 200 ?
    jr c,_under200      ; not 200+
    sub a, 200
    ld (base),a         ; sub 200 and save

    ld a, '2'           ; 2 in ascii
    rst.lil $10         ; print out a '200' digit

    jr _under100

_under200:
    cp 100              ; are we under 100 ?
    jr c,_under100      ; not 200+
    sub a, 100
    ld (base),a         ; sub 100 and save

    ld a, '1'           ; 1 in ascii
    rst.lil $10         ; print out a '100' digit

_under100:
    ld a, (base)        ; get last 2 digits as decimal
    ld c,a              ; store numerator in C
    ld d, 10            ; D will be denominator
    call C_Div_D        ; divide C by 10 to get two parts. 
                        ; A is the remainder, C is the int of C/D

    ld b, a             ; put remainder ascii into B

    ld a, c             ; get int div
    cp 0                ; if 0 (ie, number was <10)
    jr z, _lastBut1    ; just do last digit

    add a, 48           ; add 48 to make ascii of int C/D
    rst.lil $10         ; print out 10s digit
    jr _lastDigit

_lastBut1:
    add a, 48           ; add 48 to make ascii of int C/D
    rst.lil $10         ; print out 10s digit

_lastDigit:
    ld a,b              ; get remainder back
    add a, 48           ; add 48 to remainder to convert to ascii   
    rst.lil $10         ; print out last digit

    ret 

base:   .db     0       ; used in calculations

; -----------------

C_Div_D:
;Inputs:
;     C is the numerator
;     D is the denominator
;Outputs:
;     A is the remainder
;     B is 0
;     C is the result of C/D
;     D,E,H,L are not changed
;
    ld b,8              ; B is counter = 8
    xor a               ; [loop] clear flags
    sla c               ; C = C x 2
    rla                 ; A = A x 2 + Carry
    cp d                ; compare A with Denominator
    jr c,$+4            ; if bigger go to loop
    inc c               ; inc Numerator
    sub d               ; A = A - denominator
    djnz $-8            ; go round loop
    ret                 ; done 8 times, so return

; not sure how this works, need to get my head around it!
; ---------------------------------


debugA:				; debug A to screen as HEX byte pair at pos BC
	push af 
	ld (debug_char), a	; store A
				; first, print 'A=' at TAB 36,0
	ld a, 31		; TAB at x,y
	rst.lil $10
	ld a, b			; x=b
	rst.lil $10
	ld a,c			; y=c
	rst.lil $10		; put tab at BC position

	ld a, (debug_char)	; get A from store, then split into two nibbles
	and 11110000b		; get higher nibble
	rra
	rra
	rra
	rra			; move across to lower nibble
	add a,48		; increase to ascii code range 0-9
	cp 58			; is A less than 10? (58+)
	jr c, nextbd1		; carry on if less
	add a, 7		; add to get 'A' char if larger than 10
nextbd1:	
	rst.lil $10		; print the A char

	ld a, (debug_char)	; get A back again
	and 00001111b		; now just get lower nibble
	add a,48		; increase to ascii code range 0-9
	cp 58			; is A less than 10 (58+)
	jp c, nextbd2		; carry on if less
	add a, 7		; add to get 'A' char if larger than 10	
nextbd2:	
	rst.lil $10		; print the A char
	
	ld a, (debug_char)
	pop af 
	ret			; head back

debug_char: 	.db 0

; ---------------------------------------------
;
;	TEXT AND DATA	
;
; ---------------------------------------------

text_data:

	.db 31, 0, 0, "Midi reader"
	.db 31, 0, 2, "Note:"
	.db 31, 0, 4, "Value:"
	.db 31, 0, 6, "freq:"
	.db 31, 0, 10, "offset:"

end_text_data:

; set tab to B,C
setTAB:
	push af
	ld a, 31  
	rst.lil $10    ; send text colour comd
	ld a, b  
	rst.lil $10    ; send text colour comd
	ld a, c  
	rst.lil $10    ; send text colour comd
	pop af
	ret

; ---------------------------------------------
;
;	PORT CONSTANTS - FOR REFERENCE, NOT ALLL ARE USED
;
; ---------------------------------------------

PORT:			EQU	$D0		; UART1
				
REG_RBR:		EQU	PORT+0		; Receive buffer
REG_THR:		EQU	PORT+0		; Transmitter holding
REG_DLL:		EQU	PORT+0		; Divisor latch low
REG_IER:		EQU	PORT+1		; Interrupt enable
REG_DLH:		EQU	PORT+1		; Divisor latch high
REG_IIR:		EQU	PORT+2		; Interrupt identification
REG_FCT:		EQU	PORT+2;		; Flow control
REG_LCR:		EQU	PORT+3		; Line control
REG_MCR:		EQU	PORT+4		; Modem control
REG_LSR:		EQU	PORT+5		; Line status
REG_MSR:		EQU	PORT+6		; Modem status
REG_SCR:		EQU 	PORT+7		; Scratch
TX_WAIT:		EQU	16384 		; Count before a TX times out
UART_LSR_ERR:		EQU 	$80		; Error
UART_LSR_ETX:		EQU 	$40		; Transmit empty
UART_LSR_ETH:		EQU	$20		; Transmit holding register empty
UART_LSR_RDY:		EQU	%01		; Data ready


; ---------------------------------------------
;
;	NOTE data
;
; ---------------------------------------------


midiFrequencies:
 
     .dw 0    ; 0    
     .dw 0    ; 1    
     .dw 0    ; 2    
     .dw 0    ; 3    
     .dw 0    ; 4    
     .dw 0    ; 5    
     .dw 0    ; 6    
     .dw 0    ; 7    
     .dw 0    ; 8    
     .dw 0    ; 9    
     .dw 0    ; 10    
     .dw 15    ; 11    Cb0
     .dw 16    ; 12    C0
     .dw 17    ; 13    Db0
     .dw 18    ; 14    D0
     .dw 19    ; 15    Eb0
     .dw 19    ; 15    D#0
     .dw 20    ; 16    E0
     .dw 21    ; 17    F0
     .dw 23    ; 18    Gb0
     .dw 24    ; 19    G0
     .dw 25    ; 20    Ab0
     .dw 27    ; 21    A0
     .dw 29    ; 22    A#0
     .dw 32    ; 24    B#0
     .dw 34    ; 25    C#1
     .dw 36    ; 26    D1
     .dw 38    ; 27    D#1
     .dw 41    ; 28    E1
     .dw 43    ; 29    E#1
     .dw 46    ; 30    F#1
     .dw 48    ; 31    G1
     .dw 51    ; 32    G#1
     .dw 55    ; 33    A1
     .dw 58    ; 34    A#1
     .dw 61    ; 35    B1
     .dw 65    ; 36    B#1
     .dw 69    ; 37    C#2
     .dw 73    ; 38    D2
     .dw 77    ; 39    D#2
     .dw 82    ; 40    E2
     .dw 87    ; 41    E#2
     .dw 92    ; 42    F#2
     .dw 97    ; 43    G2
     .dw 103    ; 44    G#2
     .dw 110    ; 45    A2
     .dw 116    ; 46    A#2
     .dw 123    ; 47    B2
     .dw 130    ; 48    B#2
     .dw 138    ; 49    C#3
     .dw 146    ; 50    D3
     .dw 155    ; 51    D#3
     .dw 164    ; 52    E3
     .dw 174    ; 53    E#3
     .dw 185    ; 54    F#3
     .dw 196    ; 55    G3
     .dw 207    ; 56    G#3
     .dw 220    ; 57    A3
     .dw 233    ; 58    A#3
     .dw 246    ; 59    B3
     .dw 261    ; 60    B#3
     .dw 277    ; 61    C#4
     .dw 293    ; 62    D4
     .dw 311    ; 63    D#4 
     .dw 329    ; 64    E4
     .dw 349    ; 65    E#4
     .dw 369    ; 66    F#4
     .dw 392    ; 67    G4
     .dw 415    ; 68    G#4
     .dw 440    ; 69    A4
     .dw 466    ; 70    A#4
     .dw 493    ; 71    B4
     .dw 523    ; 72    B#4
     .dw 554    ; 73    C#5
     .dw 587    ; 74    D5
     .dw 622    ; 75    D#5
     .dw 659    ; 76    E5
     .dw 698    ; 77    E#5
     .dw 739    ; 78    F#5
     .dw 783    ; 79    G5
     .dw 830    ; 80    G#5
     .dw 880    ; 81    A5
     .dw 932    ; 82    A#5
     .dw 987    ; 83    B5
     .dw 1046    ; 84    B#5
     .dw 1108    ; 85    C#6
     .dw 1174    ; 86    D6
     .dw 1244    ; 87    D#6
     .dw 1318    ; 88    E6
     .dw 1396    ; 89    F6
     .dw 1480    ; 90    F#6
     .dw 1568    ; 91    G6
     .dw 1661    ; 92    G#6
     .dw 1760    ; 93    A6
     .dw 1864    ; 94    A#6
     .dw 1975    ; 95    B6
     .dw 2093    ; 96    B#6
     .dw 2217    ; 97    C#7
     .dw 2349    ; 98    D7
     .dw 2489    ; 99    D#7
     .dw 2637    ; 100    E7
     .dw 2793    ; 101    E#7
     .dw 2960    ; 102    F#7
     .dw 3136    ; 103    G7
     .dw 3322    ; 104    G#7
     .dw 3520    ; 105    A7
     .dw 3729    ; 106    A#7
     .dw 3951    ; 107    B7
     .dw 4186    ; 108    B#7
     .dw 4434    ; 109    C#8
     .dw 4698    ; 110    D8
     .dw 4978    ; 111    D#8
     .dw 5274    ; 112    E8
     .dw 5587    ; 113    E#8
     .dw 5919    ; 114    F#8
     .dw 6271    ; 115    G8
     .dw 6644    ; 116    G#8
     .dw 7040    ; 117    A8
     .dw 7458    ; 118    A#8
     .dw 7902    ; 119    B8
     .dw 8372    ; 120    B#8
     .dw 8869    ; 121    C#9
     .dw 9397    ; 122    D9
     .dw 9956    ; 123    D#9
     .dw 10548    ; 124    E9
     .dw 11175    ; 125    E#9
     .dw 11840    ; 126    F#9
     .dw 12544    ; 127    G9
     .dw 13290    ; 128    G#9
     .dw 14080    ; 129    A9
     .dw 14917    ; 130    A#9
     .dw 15804    ; 131    B9
     .dw 16744    ; 132    B#9