;	Serial MIDI Synth PLayer
; 	Richard Turnnidge 2024

; 	Reads MIDI serial data from keyboard and plays sounds

; v0.12 better uart handling of three byte array
; v0.14 new keyboard routine
; v0.16 don't play a channel which is still playing
; v0.18 screen mode, keyboard crash fix
; v0.19 colour and layout fixes
; v0.20 add multiple instruments
; v0.22 file save/load moved across
; v0.23 started to add recording of tunes
; v0.24 start to add playback of songs
; v0.25 added new baud rate for hardware adapter
; v0.26 better segredation of first data byte
; v0.28 Interrupt driven serial buffer - not fully working, but data is being stored in buffer
; v0.29 trying to fix interrupts -> new data

; ---------------------------------------------
;
;	CONSTANTS
;
; ---------------------------------------------

midi_ID:			equ	147 		; Keystation keyboard ID
maxChannels:		equ 8 			; max sound channels to use
baud_rate:			equ 31250		; baud rate to use
mos_getkbmap:		EQU	1Eh			; routine to get keyboard matrix

;	colours used

col_title:			EQU	9			; colour spec
col_words:			EQU	7			; colour spec
col_values:			EQU	15			; colour spec
col_copy:			EQU	9			; colour spec

;	UART CONSTANTS - FOR REFERENCE, NOT ALL ARE USED

PORT:			EQU	$D0			; UART1
				
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
REG_SCR:		EQU PORT+7		; Scratch
TX_WAIT:		EQU	16384 		; Count before a TX times out
UART_LSR_ERR:	EQU $80			; Error
UART_LSR_ETX:	EQU $40			; Transmit empty
UART_LSR_ETH:	EQU	$20			; Transmit holding register empty
UART_LSR_RDY:	EQU	%01			; Data ready

; ---------------------------------------------
;
;	MACROS
;
; ---------------------------------------------

	macro MOSCALL afunc
	ld a, afunc
	rst.lil $08
	endmacro

	macro TABTO x,y
	ld a, 31  
	rst.lil $10    
	ld a, x  
	rst.lil $10   
	ld a, y  
	rst.lil $10    
	endmacro

	macro setcol col
	ld a, 17  
	rst.lil $10    
	ld a, col  
	rst.lil $10    
	endmacro

	macro hiliteNote whichNote
	ld b, 19
	ld c, 4
	call setTAB

	ld a, col_words
	call setCOL
	ld a, ' '
	rst.lil $10

	ld b, 19
	ld c, 6
	call setTAB
	ld a, col_words
	call setCOL
	ld a, ' '
	rst.lil $10

	ld b, 19
	ld c, 8
	call setTAB
	ld a, col_words
	call setCOL
	ld a, ' '
	rst.lil $10

	ld b, 19
	ld c, 10
	call setTAB
	ld a, col_words
	call setCOL
	ld a, ' '
	rst.lil $10

	ld b, 19
	ld c, whichNote * 2 + 2
	call setTAB
	ld a, col_values
	call setCOL
	ld a, '>'
	rst.lil $10

	endmacro

	macro setupchannel chNum
	.db 23, 0, $85, chNum, 8			; enable channel

	.db 23, 0, $85, chNum, 2, 127		; volume channel

	.db 23, 0, $85, chNum, 4, -1		; set ch to sample mode and which sample (-1)

	.db 23, 0, $85, chNum, 6, 1			; set ch0 envelope
	.dw 30								; Attack
	.dw 120								; Decay
	.db 64								; Sustain
	.dw 600								; Release
	endmacro

; ---------------------------------------------
;
;	INITIALISE
;
; ---------------------------------------------

	.assume adl=1						; big memory mode
	.org $40000							; load code here

	jp start_here						; jump to start of code

	.align 64							; MOS header
	.db "MOS",0,1

	include "keys.inc"

; ---------------------------------------------
;
;	INITIAL SETUP CODE HERE
;
; ---------------------------------------------

start_here:
										; store everything as good practice	
	push af								; pop back when we return from code later
	push bc
	push de
	push ix
	push iy

	im 2  								; set interrupt mode 2 for the uart

	call CLS 							; clear screen
	call openUART1						; init the UART1 serial port


	ld hl, text_data
	ld bc, end_text_data - text_data
	rst.lil $18							; print default text to screen

	call loadSample
	call resetList
	call hidecursor						; hide the cursor
	call showPlaying

	hiliteNote 1

	ld a, 0
	ld (amPlaying), a 
	ld (amRecording), a 				; reset to 0
	ld (numRecords), a 				; reset to 0

	ld a, -1 							; default instrument
	call setInstrument

	call clearBuffer					; clear uart buffer

; ---------------------------------------------
;
;	MAIN LOOP
;
; ---------------------------------------------

MAIN_LOOP:	
	MOSCALL $08							; get IX pointer to sysvars
	ld a, (ix + 05h)					; ix+5h is 'last key pressed'
	cp 27								; is it ESC key?
	jp z, exit_here						; if so exit cleanly

    MOSCALL mos_getkbmap
    ld a, (ix + $00)                    ; A is mask of current keyboard. DD 7E NN
    bit 3, a 							; SHIFT L
    jp nz, ctrl_pressed



_ctrl_return:
	call checkInstrumentChange			; see if we change instrument
;	call midiChecker					; get any new data from UART1 - not used now with interrupts
	call bufferChecker					; get any new data from UART1 buffer
	call checkKeyboard					; get new data from Agon keyboard

; 	call display_buffer

	ld a, (amPlaying)
	cp 1
	call z, checkNextNote				; get next note and see we should deal with it

	ld a, (midi_received)				; check if we got anything
	cp 0
	jr z, MAIN_LOOP						; nothing new, loop round again			

	call manageData						; deal with data

 ;	call showPlaying
 	

	jp MAIN_LOOP

; ---------------------------------------------

ctrl_pressed:

    MOSCALL mos_getkbmap
    ld a, (ix + $04)                    ; A is mask of current keyboard. DD 7E NN
    bit 3, a 							; T key reset time
    call nz, resetSystime

    MOSCALL mos_getkbmap
    ld a, (ix + $0A)                    ; A is mask of current keyboard. DD 7E NN
    bit 6, a 							; L for Load key
    call nz, LOAD_SONG

    MOSCALL mos_getkbmap
    ld a, (ix + $0A)                    ; A is mask of current keyboard. DD 7E NN
    bit 1, a 							; S key
    call nz, SAVE_SONG

    MOSCALL mos_getkbmap
    ld a, (ix + $06)                    ; A is mask of current keyboard. DD 7E NN
    bit 3, a 							; X key  to stop recording
    call nz, START_RECORDING

   MOSCALL mos_getkbmap
    ld a, (ix + $08)                    ; A is mask of current keyboard. DD 7E NN
    bit 2, a 							; X key  to stop recording
    call nz, STOP_RECORDING

   MOSCALL mos_getkbmap
    ld a, (ix + $06)                    ; A is mask of current keyboard. DD 7E NN
    bit 7, a 							; P key  to stop recording
    call nz, PLAY_SONG

   MOSCALL mos_getkbmap
    ld a, (ix + $0C)                    ; A is mask of current keyboard. DD 7E NN
    bit 2, a 							; SPACE key  to stop playing
    call nz, STOP_SONG

	jp  _ctrl_return

; ---------------------------------------------

checkNextNote:
	MOSCALL $08					; get IX pointer to sysvars. ix is at the position we want
								; ix +0, +1 and +2

	ld hl, 0
	ld bc, 0

	ld a, (ix + 0)				; lowest time byte
	ld l, a
	ld a, (ix + 1)				; middle time byte
	ld h, a  					; HL is now current time


	ld a, (iy + 0)				; lowest time byte
	cp 255
	jp z, STOP_SONG  			; hit a 255 so stop


	ld c, a
	ld a, (iy + 1)				; middle time byte
	ld b, a  					; BC is now next time stamp for action

	sbc hl, bc 					; compare the two. Current - next note time

	ret c  						; if not there yet then return

	; ok time to deal with next note

	ld a, (iy + 4)				; note to play
	ld (midiCommand + 1), a  	; store next note

	ld a, (iy + 5)				; volocity to play
	ld (midiCommand + 2), a  	; store next velocity

	ld a, 1
	ld (midi_received), a

	inc iy 
	inc iy 
	inc iy 
	inc iy 
	inc iy 
	inc iy 						; set pointer to next note data

	ret  

; ---------------------------------------------

display_buffer:						; display the buffer contents

	TABTO 20, 24  					; set start position

	ld a, (numRecords)
 	call printDec 

	ld hl, LINEFEED					; pointer to newline char sequence
	call printString				; print a new line

	TABTO 0, 30  					; set start position

	ld de, serialBuffer				; HL is start of buffer bytes
	ld b, 24					; will go through first 24 bytes only as example

@loop:

	ld a, (de)					; get buffer byte
	inc de   					; inc for next time
	push bc  					; store counter
	push de  					; store position into buffer

	call printDec 					; print the contants of buffer position x

	ld hl, LINEFEED					; pointer to newline char sequence
	call printString				; print a new line

	pop de  					; get position into buffer back
	pop bc  					; get B counter back
	djnz @loop					; go round again if not done all 24

	ret




; ---------------------------------------------
;
;	EXIT CODE CLEANLY
;
; ---------------------------------------------

exit_here:
	call stopAll
	call closeUART1
	call CLS
								; reset all values before returning to MOS
	call showcursor

	ld a, 17
	rst.lil $10
	ld a, 15	
	rst.lil $10					; return ink to bright white


	pop iy
	pop ix
	pop de
	pop bc
	pop af
	ld hl,0

	ret							; return to MOS here


; ---------------------------------------------
;
;	ROUTINES
;
; ---------------------------------------------

checkInstrumentChange:
	; point ix at keyboard matrix
    MOSCALL mos_getkbmap
    ; check instrument 1
	ld a, (ix + $06)                     
	bit 0, a 
	jr z, @f

	hiliteNote 1

	ld a, -1
	jp setInstrument

@@:

    ; check instrument 2
	ld a, (ix + $06)                     
	bit 1, a 
	jr z, @f

	hiliteNote 2


	ld a, -2
	jp setInstrument

@@:

    ; check instrument 3
	ld a, (ix + $02)                     
	bit 1, a 
	jr z, @f

	hiliteNote 3


	ld a, -3
	jp setInstrument

@@:

    ; check instrument 4
	ld a, (ix + $02)                     
	bit 2, a 
	jr z, @f

	hiliteNote 4


	ld a, -4
	jp setInstrument

@@:


	ret


setInstrument:

	ld (inst), a 						; set the waveform to use

	ld a, 0								; counter through all 8 channels 0-7
setLoop:
	ld (chn3), a  						; set channel number
	ld (chn1), a  						; set channel number
	ld (chn2), a  						; set channel number
	ld (chn4), a  						; set channel number
	ld (chn5), a  						; set channel number
	push af 

	ld hl, startInstument
	ld bc, endInstument - startInstument
	rst.lil $18

	pop af
	inc a 								; inc counter
	cp 8  								; have we completed all 8?
	jr nz, setLoop						; if not loop round again

	push af
	call wait_for_release
	pop af 

	ret 


startInstument:


		.db     23,0,85h                ; audio command
chn1:	.db		0
		.db 	10						; reset channel

		.db 23, 0, $85
chn2:	.db 0
		.db 8							; enable channel

		.db 23, 0, $85
chn3:	.db 0
		.db 2, 127						; volume channel

; set waveform of channel
		.db     23,0,85h                ; audio command
chn4:	.db     0                       ; channel
		.db 	4						; set waveform
inst:	.db     -1                      ; waveform id


; set envelope of channel
		.db     23,0,85h                ; audio command
chn5:	.db     0                       ; channel
		.db 	6, 1					; set envelope type
		.dw 30							; Attack
		.dw 120							; Decay
		.db 64							; Sustain
		.dw 600							; Release



endInstument:

; ---------------------------------------------

wait_for_release:	; wait for key up state so we only do it once
	MOSCALL $08		; get IX pointer to sysvars
	ld a, (ix + 18h)	; get key state
	cp 0	; are keys up, none currently pressed?
	jr nz, wait_for_release
	ret  

; ---------------------------------------------

resetList:								; reset all channels to 0
	ld a, 0
	ld (midi_received), a 				; reset recEIver flag

	ld hl, channelData					; resrt chanal data
	ld (hl), 0
	ld de, channelData+1
	ld bc, 8
	ldir
	ret

; ---------------------------------------------
; display channels - routine to display playing channels and thier notes
; only shows 2 digits as keyboard does not go above 99

showPlaying:

	TABTO 0,12 							; start at 0, 16 position

	ld b, maxChannels  					; loop through (8) channels
	ld hl, channelData					; where we store the note for each channel

_showLoop:
	ld a, 17
	rst.lil $10
	ld a, col_words
	rst.lil $10							; set ink colour

	ld a, maxChannels
	sub b 								; get from 0-8
	add a, 48							; turn to ascii digit
	rst.lil $10							; print digit
	ld a, ' '
	rst.lil $10							; print space
	rst.lil $10							; print space


	ld a, 17
	rst.lil $10
	ld a, col_values
	rst.lil $10							; set ink colour

	ld a, (hl)							; get note for channel
	inc hl 								; inc channel pointer

	push hl
	push bc  

	call printDec						; print dec value of note
	ld hl, LINEFEED
	call printString					; print CR

	pop bc
	pop hl

	djnz _showLoop						; loop 8 times


	ret


; ---------------------------------------------
; deal with incoming data

manageData:
	ld a, 0
	ld (midi_received), a 					; reset new midi flag


; 	jp here						;; TESTING


	; need all new here based on three byte array: 
	; Device ID
	; Note
	; HitPower (0 = release)
	call storeTimestamp


	ld a, (amRecording)
	cp 1
	call z, RECORD_NOTE

	TABTO 11, 2

	ld a, (midiCommand)  					; byte 1 of array, device ID

	call printDec						; print device ID
	ld hl, LINEFEED
	call printString					; print CR

	TABTO 11, 4

	ld a, (midiCommand+1)  						; byte 2 of array, midi note
	ld (MidiNumber), a 					; store it
	call printDec						; print midi note


	TABTO 11, 6

	ld hl, blanks
	call printString

	TABTO 11, 6


	ld a, (midiCommand+2)  						; byte 3 of array, velocity
	ld (MidiValue), a 					; store it

	call printDec						; print hit power or release (0)



	ld a, (MidiValue)  					; velocity
	cp 0
	jr z, didRelease					; was 0 so a key release

	call startSound						; else was a start sound command


	ret


didRelease:

	call stopSound

	ret


; ---------------------------------------------
; start a sound playing

startSound:

	ld a, (MidiNumber)					; this is midi number
	ld (midiCode), a  					; store in case we need it later
	add a, a							; x2
	ld (midiOffset), a  				; store offset in case we need it

	call playMidiNote

	TABTO 11, 8
	ld hl, blanks
	call printString

	TABTO 11, 8

	ld hl, 0
	ld a, (noteFrequency)
	ld l, a
	ld a, (noteFrequency + 1)
	ld h, a 


	call B2D16

	LD HL,B2DEND-4
	ld a, (hl)
	cp ' '
	jr nz, notSpace
	inc hl
	ld a, (hl)
	cp ' '
	jr nz, notSpace
	inc hl
notSpace:	
    CALL printString

	ret

; ---------------------------------------------

playMidiNote:

	; get frequency to play

	ld HL, midiFrequencies					; list of frequencies for each midi note
	ld de, 0								; reset to 0
	ld e, a 								; the offset
	add hl, de 								; get offset into data

	ld a, (hl)
	ld (noteFrequency), a  					; set LSB

	inc hl
	ld a, (hl)
	ld (noteFrequency + 1), a  				; set MSB

	; find a free channel
	; start at 0
	; channelData has list of currently active channels

	ld bc, 0
	ld hl, channelData


_checkingLoop:	
 	ld a, (hl)								; check if 0 playing

	cp 0
	jr z, _allOK

	inc c 
	inc hl 
	ld a, c 
	cp 8
	jr nz, _checkingLoop 					; loop if not tried all

	; bail here if no free channels
	ret

_allOK:

	; ------------------------
	ld a, c 

 	ld (channelToPlay), a 					; play the note

	ld de, 0
	ld e,a
	ld hl, channelData

	add hl, de  							; get offset to channel data storage
	ld a,(MidiNumber)
	ld (hl), a  							; Store note playing on that channel

 	call showPlaying


	ld hl, playNote
	ld bc, endPlayNote - playNote
	rst.lil $18

	ret

; ---------------------------------------------

playNote:									; data to send to VDP to play a note
		.db 23, 0, $85 
channelToPlay: 	.db 0						; which channel to play, set in code
		.db 0, 127							; play note
noteFrequency:	.dw $0						; set in code
theLength:	.dw -1							; loop forever

endPlayNote:

; ---------------------------------------------

stopAll:
	ld a, 0

_stopLoop:
	push af
	
	ld (stopChannel), a
	
	ld hl, stopNote
	ld bc, endStopNote - stopNote
	rst.lil $18

	pop af
	inc a 
	cp 8
	jr nz, _stopLoop

	ld a, 0
	ld ix, channelData
	ld (ix +0), a
	ld (ix +1), a
	ld (ix +2), a
	ld (ix +3), a
	ld (ix +4), a
	ld (ix +5), a
	ld (ix +6), a
	ld (ix +7), a

	call showPlaying
	
	ret 

; ---------------------------------------------
; need to add which channel

stopSound:

	ld a, (MidiNumber)							; the note we are looking to stop
	ld c, a										; put it in C. Need to check aganst each channel

	ld hl, channelData							; where we store the note for each channel
	ld b, maxChannels							; number of channels to check

_stopLoop2:

	ld a, (hl)									; get note in channel + offset
	cp c 										; check against C
	jr z, _gotNote								; if a match, then jp
	inc hl

	djnz _stopLoop2

	ret  										; if we got here, it didn't exist, which is unlikely

_gotNote:
	ld (hl), 0
	ld a, maxChannels							; 8
	sub b 										; get channel number

	ld (stopChannel), a


	ld a, 0
	ld (waitingMidiNumber), a  					; reset waitingMidiNumber
	ld (waitingMidiValue), a  					; reset waitingMidiValue
	ld (is_playing), a

	ld hl, stopNote
	ld bc, endStopNote - stopNote
	rst.lil $18

	call showPlaying
	ret 


; ---------------------------------------------
; silence a channel

stopNote:

		.db 23, 0, $85
stopChannel:	.db 0							; which channel
		.db 2, 0								; silence channel

endStopNote:


; ---------------------------------------------
;
;	AUDIO INITIALISATION
;
; ---------------------------------------------

; loads sample of instrument and sets up channels

loadSample:                             	; for loading audio samples

	ld hl, sampleStr                    	; start of data to send
	ld bc, endSampleStr - sampleStr     	; length of data to send
	rst.lil $18                         	; send data
	ret 

sampleStr:

; load sample 1
	.db     23,0,85h                    	; audio command
	.db     -1, 5                       	; sample number, sample management
	.db     0                           	; load
	.dl     6246                       		; length in bytes (LONG)
	incbin "synth.raw"

; set baseline of freq, ie, middle C 264
	.db     23,0,85h                    	; audio command
	.db     -1, 5                       	; sample number, sample management
	.db     3                           	; set baseline freq
	.dw     264								; the base freq

; ; load sample 2
; 	.db     23,0,85h                    	; audio command
; 	.db     -2, 5                       	; sample number, sample management
; 	.db     0                           	; load
; 	.dl     18171                       	; length in bytes (LONG)
; 	incbin "violin.raw"

; ; set baseline of freq, ie, middle C 264
; 	.db     23,0,85h                    	; audio command
; 	.db     -2, 5                       	; sample number, sample management
; 	.db     3                           	; set baseline freq
; 	.dw     264								; the base freq


; ; load sample 3
; 	.db     23,0,85h                    	; audio command
; 	.db     -3, 5                       	; sample number, sample management
; 	.db     0                           	; load
; 	.dl     19615                       	; length in bytes (LONG)
; 	incbin "brasspad.raw"

; ; set baseline of freq, ie, middle C 264
; 	.db     23,0,85h                    	; audio command
; 	.db     -3, 5                       	; sample number, sample management
; 	.db     3                           	; set baseline freq
; 	.dw     264								; the base freq

; ; load sample 4
; 	.db     23,0,85h                    	; audio command
; 	.db     -4, 5                       	; sample number, sample management
; 	.db     0                           	; load
; 	.dl     5608                       	; length in bytes (LONG)
; 	incbin "piano.raw"

; ; set baseline of freq, ie, middle C 264
; 	.db     23,0,85h                    	; audio command
; 	.db     -4, 5                       	; sample number, sample management
; 	.db     3                           	; set baseline freq
; 	.dw     264								; the base freq



; enable channels 0-7. 0-2 should be on by default anyway

	setupchannel 0
	setupchannel 1
	setupchannel 2
	setupchannel 3
	setupchannel 4
	setupchannel 5
	setupchannel 6
	setupchannel 7

endSampleStr:

; ---------------------------------------------
;
;	UART CODE
;
; ---------------------------------------------

bufferChecker:										; check for 3 or more bytes in buffer and pass to music player

	ld a, 0
	ld (midi_received), a  							; clear flag

	ld a, (numRecords)								; get number of items in uart buffer
	cp 3 											; are there 3 or more?
	ret c 											; return if less than 3

	; get three bytes and allocate to the midi	
	; need to diasble interrupts or may get a conflict

	di 												; disable interrupts so as not to get mixed up mid code

	; grab first 3 bytes as use as new midi data

	ld a, (serialBuffer)							; byte 1
	ld (midiCommand), a 							; store first byte
	ld a, (serialBuffer + 1)						; byte 2
	ld (midiCommand + 1), a  						; store it in 2nd array position
	ld a, (serialBuffer + 2)						; byte 3
	ld (midiCommand + 2), a  						; store it in 3rd array position

	; shift next lot of data 3 bytes forward to front of list

	ld hl, serialBuffer + 3	 						; next set of bytes start 3 bytes later	
	ld de, serialBuffer								; move to start of table
	ld bc, 253
	ldir 											; shift all buffer down 3 places

	; reduce the number of records as we just removed 3

	ld a, (numRecords)								; get number of items in uart buffer
	sub 3											; reduce count by 3
	ld (numRecords), a  							; store new total

	; shift buffer write position down as we just removed 3

	ld hl,(uart_buf_pos)							; get curent buffer write position
	dec hl											; dec for next time
	dec hl											; dec for next time
	dec hl											; dec for next time x 3
	ld (uart_buf_pos), hl  							; store pointer to buffer position

	; set new midi data flag

	ld a, 1
	ld (midi_received), a  							; set flag that we have new three byte array of midi data

	ei 												; enable interrupts

	ret

; ---------------------------------------------


midiChecker:

	; *** NOT USED with interrupt driven uart ***

	; need to check for new uart data. When received, if device ID, then expect three bytes in total to store.

	; *** We are missing some UP notifications ***

	call uart1_handler
	ld a, (uart1_received)
	cp 0
	ret z  								; nothing received, so return

	;ld hl, midiCommand					; pointer to array to be stored
	ld a, (uart1_buffer)				; byte received
	ld (midiCommand), a 							; store first byte
	;inc hl
	cp midi_ID						; check A if our device?
; 	bit 7, a 							; is it > 127. All devices ar 128+
; 	ret z 								; no, spurious data
	ret nz 								; no, spurious data

_waitByte2:
	call uart1_handler
	ld a, (uart1_received)
	cp 0
	jr z, _waitByte2					; wait in loop 2 for second byte

	ld a, (uart1_buffer)				; got another byte

  	cp midi_ID
  	jp z, stopAll

; 	call z, waitingHere;midiChecker ;waitingHere					; bail to checker if 147

 ;	bit 7, a 							; is it > 127. All devices ar 128+
; 	ret nz 								; yes, spurious data
;	jp nz, stopAll

; 	bit 7, a 							; is it > 127. All devices ar 128+
; 	ret nz 								; yes, spurious data

	ld (midiCommand + 1), a  						; store it in 2nd array position
	;inc hl 

_waitByte3:
	call uart1_handler
	ld a, (uart1_received)
	cp 0
	jr z, _waitByte3					; wait in loop 3 for second byte

	ld a, (uart1_buffer)				; got another byte

 	cp midi_ID
 	jp z, stopAll

; 	bit 7, a 							; is it > 127. All devices ar 128+
; 	ret nz 								; yes, spurious data
;	jp nz, stopAll
;
	ld (midiCommand + 2), a  						; store it in 3rd array position

	ld a, 1
	ld (midi_received), a  				; set flag that we have new three byte array of midi data

	ret 



waitingHere:

	; print some data for display


	TABTO 21, 20

	ld a, (midiCommand)  			
	call printDec						

	TABTO 21, 21

	ld a, (midiCommand + 1)  			
	call printDec						

	TABTO 21, 22

	ld a, (midiCommand + 2)  			
	call printDec						

ww:

	MOSCALL $08							; get IX pointer to sysvars
	ld a, (ix + 05h)					; ix+5h is 'last key pressed'
	cp '0'								; is it '0' key?
	ret z								; if so return

	jr ww



; ---------------------------------------------

clearBuffer:
	; do this before starting to clear any unwanted data

	call uart1_handler
	ld a, (uart1_received)
	cp 0
	ret z  								; nothing received, so return


	jr clearBuffer						; loop rpound until nothing left in buffer



; ---------------------------------------------

uart1_handler:							; now only using at start to make sure buffer is clear before listening
	DI
	PUSH	AF
	IN0	A,(REG_LSR)						; Get the line status register
	AND	UART_LSR_RDY					; Check for characters in buffer
	JR	Z, noData 						; Nothing received
			
	LD	A,1   							; we got new data
	LD	(uart1_received),a 				; so set flag for new data
	IN0	A,(REG_RBR)						; Read the character from the UART receive buffer
	LD	(uart1_buffer),A  				; store new byte of data
	POP	AF
	EI

	RET

noData:
	XOR 	A,A  						; quick way of A = 0
	LD	(uart1_received),A				; note that nothing is available	
	POP	AF
	EI

	RET
			

; ---------------------------------------------

openUART1:
	ld ix, UART1_Struct
	MOSCALL $15							; open uart1

   	ld hl, uart_isr_handler
	ld e, $1A        					; interrupt number (UART1)
	ld a, $14     						; mos_api_setintvector
	rst.lil $08


	ld hl, serialBuffer  				; get start of uart buffer
	ld (uart_buf_pos), hl				; set start buffer write position

	ret 

; ---------------------------------------------

closeUART1:
	MOSCALL $16 					; close uart1
	ret 

; ---------------------------------------------

UART1_Struct:	
	.dl 	31250; baud_rate			; baud (stored as three byte LONG)
	.db 	8 							; data bits
	.db 	1 							; stop bits
	.db 	0 							; parity bits
	.db 	0							; flow control
	.db 	00000001b					; interrupt bits - bit 0 is enable received data interrupt

							;    - Bit 0: Set to enable received data interrupt
							;    - Bit 1: Set to enable transmit data interrupt
							;    - Bit 2: Set to enable line status change interrupt
							;    - Bit 3: Set to enable modem status change interrupt
							;    - Bit 4: Set to enable transmit complete interrupt

; ---------------------------------------------

uart_isr_handler:				; this is the interrupt service routine (ISR) that gets called
	di							; stop any other interrupts while we do this
	push af
	push hl 					; store any registers we are going to use

	; do ISR code here

	in0 a,(REG_RBR)				; read byte from buffer

	ld hl,(uart_buf_pos)		; get curent buffer write position
	ld (hl), a 					; store byte received
	inc hl						; inc for next time
	ld (uart_buf_pos), hl  		; store pointer to buffer position

	ld a, (numRecords)
	inc a
	cp 254
	jr z, @f
	ld (numRecords),a  			; just counting the bytes coming in
@@:
	pop hl
	pop af						; return register values
	ei							; enable interrupts again

	reti.l  					; return from ISR


uart_buf_pos:	.dl 	0		; current position into buffer memory


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

showcursor:
	push af
	ld a, 23
	rst.lil $10
	ld a, 1
	rst.lil $10
	ld a,1
	rst.lil $10	;VDU 23,1,0
	pop af
	ret


; ---------------------------------------------

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

    ld a,' '
    rst.lil $10         ; print extra space to clear old stuff


    ret 

base:   .db     0       ; used in calculations

; ---------------------------------

debugA:								; debug A to screen as HEX byte pair at pos BC
	push af 
	push bc
	push hl
	ld (debug_char), a				; store A
									; first, print 'A=' at TAB 36,0
	ld a, 31						; TAB at x,y
	rst.lil $10
	ld a, b							; x=b
	rst.lil $10
	ld a,c							; y=c
	rst.lil $10						; put tab at BC position

	ld a, (debug_char)				; get A from store, then split into two nibbles
	and 11110000b					; get higher nibble
	rra
	rra
	rra
	rra								; move across to lower nibble
	add a,48						; increase to ascii code range 0-9
	cp 58							; is A less than 10? (58+)
	jr c, nextbd1					; carry on if less
	add a, 7						; add to get 'A' char if larger than 10
nextbd1:	
	rst.lil $10						; print the A char

	ld a, (debug_char)				; get A back again
	and 00001111b					; now just get lower nibble
	add a,48						; increase to ascii code range 0-9
	cp 58							; is A less than 10 (58+)
	jp c, nextbd2					; carry on if less
	add a, 7						; add to get 'A' char if larger than 10	
nextbd2:	
	rst.lil $10						; print the A char
	
	ld a, (debug_char)
	pop hl
	pop bc
	pop af 
	ret								; head back

debug_char: 	.db 0

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

; Print ASCII of a BYTE, WORD, LONG, etc

; Combined routine for conversion of different sized binary numbers into
; directly printable ASCII(Z)-string
; Input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   B2D8             A                    255  (3 digits)
;   B2D16           HL                  65535   5   "
;   B2D24         E:HL               16777215   8   "
;   B2D32        DE:HL             4294967295  10   "
;   B2D48     BC:DE:HL        281474976710655  15   "
;   B2D64  IX:BC:DE:HL   18446744073709551615  20   "
;
; The resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; The number is aligned to the right, and leading 0's are replaced with spaces.
; On exit HL points to the first digit, (B)C = number of decimals
; This way any re-alignment / postprocessing is made easy.
; Changes: AF,BC,DE,HL,IX
; P.S. some examples below

; by Alwin Henseler


B2D8:    LD H,0
         LD L,A
B2D16:   LD E,0				; ENTRY point for 16 bit numbers
B2D24:   LD D,0
B2D32:   LD BC,0
B2D48:   LD IX,0          ; zero all non-used bits
B2D64:   LD (B2DINV),HL
         LD (B2DINV+2),DE
         LD (B2DINV+4),BC
         LD (B2DINV+6),IX ; place full 64-bit input value in buffer
         LD HL,B2DBUF
         LD DE,B2DBUF+1
	LD (HL), ' '

B2DFILC: EQU $-1         ; address of fill-character
         LD BC,18
         LDIR            ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1          ; no. of bytes in BCD value
         LD HL,B2DINV+8  ; (address MSB input)+1
         LD BC,#0909
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ     ; all 0: continue with postprocessing
         DEC HL
         OR (HL)         ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1   ; determine no. of most significant 1-bit
         RRA
         LD D,A          ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1  ; address LSB of BCD value
         LD B,E          ; current length of BCD value in bytes
         RL D            ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A       ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0    ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E           ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1       ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1   ; repeat for remaining bits from 1 input byte
         POP BC          ; no. of remaining bytes in input value
         LD C,8          ; reset bit-counter
         POP HL          ; pointer to byte from input value
         DEC HL
         LD D,(HL)       ; get next group of 8 bits
         DJNZ B2DLUS2    ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND    ; address of terminating 0
         LD C,E          ; size of BCD value in bytes
         OR A
         SBC HL,BC       ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL        ; HL=address BCD value, DE=start of decimal value
         LD B,C          ; no. of bytes BCD
         SLA C           ; no. of bytes decimal (possibly 1 too high)
         LD A,'0'
         RLD             ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP '0'          ; (HL) was > 9h?
         JR NZ,B2DEXPH   ; if yes, start with recording high digit
         DEC C           ; correct number of decimals
         INC DE          ; correct start address
         JR B2DEXPL      ; continue with converting low digit
B2DEXP:  RLD             ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A       ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL          ; next BCD-byte
         DJNZ B2DEXP     ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC       ; return with HL pointing to 1st decimal
         RET

B2DINV:  .DS 8            ; space for 64-bit input value (LSB first)
B2DBUF:  .DS 20           ; space for 20 decimal digits
B2DEND:  .DS 1            ; space for terminating 0




; ---------------------------------------------
;
;	TEXT AND DATA	
;
; ---------------------------------------------

text_data:

	.db 22,8 									; set screen to mode 8

	.db 17,col_title 							; ink
	.db 31, 0, 0, "Agon Midi Keyboard Synth"
	.db 17,col_words 							; ink
	.db 31, 0, 2, "Device ID:          Instruments"
	.db 31, 0, 4, "     Note:          1) Synth Dreams"
	.db 31, 0, 6, " Velocity:          2) Agon Violin"
	.db 31, 0, 8, "Frequency:          3) Magical Organ"
	.db 31, 0, 10, "Ch.Note             4) Electric Piano"


	.db 17,col_copy 	
	.db 31, 0, 24, "Use Agon keys for middle C"

	.db 17,col_words 	
	.db 31, 0, 26, " W E   T Y U "
	.db 31, 0, 27, "A S D F G H J K"


	.db 17,col_copy 							; ink
	.db 31, 0, 29, "© R. Turnnidge 2024"
	.db 17,col_values 							; ink 


end_text_data:

LINEFEED:           .asciz "\r\n"

; ---------------------------------------------

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

setCOL:
	push af
	ld a, 17  
	rst.lil $10    ; send text colour comd
	pop af
	rst.lil $10    ; send text colour comd
	ret

; ---------------------------------------------
;
;	Variables data
;
; ---------------------------------------------

blanks:				.blkb 6,32				; to erase text on screen
					.db 0					; termination

channelData:
					.blkb 8,0				; store what midi note is playing on each channel

midiCode: 			.db 0 					; current midi code received
midiOffset: 		.db 0 					; current midi code offset

uart1_buffer:		.db	1					; 64 byte receive buffer
uart1_received:		.db	1					; boolean
uart1_data:			.db	1					; final pot MidiValue received
uart1_command: 		.db 0					; current command received

midiTimeStamp:		.dl 0					; time stamp of midi command received
midiCommand: 		.db midi_ID,0,0			; three bytes for array, first set as default in case keyboard
											; keyboard does not alter it

is_playing:			.db 0					; quick hack for single not playing
waitingMidiNumber:	.db 1					; if waiting for note to arrive
waitingMidiValue:	.db 0					; if waiting for hit MidiValue to arrive

MidiNumber:			.db 0
MidiValue:			.db 0

channelCount:	.db 0						; store which cannel to use next
midi_received: 	.db 0
	
; instruments

instruments:								; sample IDs of instruments 1-4
	.db -1
	.db -2
	.db -3
	.db -4


; ---------------------------------------------
;
;	MidiNumber data
;
; ---------------------------------------------

	include "midiFrequencies.inc"

; ---------------------------------------------
;
;	ROUTINES TO MOVE ACROSS
;
; ---------------------------------------------
; File access modes
;
fa_read:			EQU	01h
fa_write:			EQU	02h
fa_open_existing:	EQU	00h
fa_create_new:		EQU	04h
fa_create_always:	EQU	08h
fa_open_always:		EQU	10h
fa_open_append:		EQU	30h

LOAD_SONG:

	call wait_for_release

	call getFileName

	ld hl, loadingStr
	ld bc, endLoading - loadingStr
	rst.lil $18						; print that we are saving


	ld de, musicData				; data to save is here
	ld hl, fileNameBuffer 			;fileName			; the file name we are to use
	ld bc, 4096						; number of bytes to save

	MOSCALL $01						; MOS load file

	ld hl, eraseStr
	ld bc, endEraseStr - eraseStr
	rst.lil $18						; ask for file

	ret

; ---------------------------------------------

STOP_RECORDING:

	ld a, 0
	ld (amRecording), a
	call resetSystime

	ld a, 255
	ld hl, (recordHead)
	ld (hl), a  					; terminate with 255

	;call resetRecordhead

	ld hl, eraseStr
	ld bc, endEraseStr - eraseStr
	rst.lil $18						; clear recording info

	ret 

; ---------------------------------------------

START_RECORDING:
	ld a, 1
	ld (amRecording), a
	call resetSystime

	call resetRecordhead

	ld hl, recordingStr
	ld bc, endRecording - recordingStr
	rst.lil $18						; print that we are recording

	ret 

recordHead: .dl 0 					; this will be where data is store

; ---------------------------------------------

resetRecordhead:

	ld iy, musicData

	ret  

; ---------------------------------------------

RECORD_NOTE:						; store note being played
	push iy
	pop de 							; get current record head position
	ld hl, 	midiTimeStamp			; grab the six bytes needed
	ld bc, 6

	ldir 

	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators
	inc de 
	ld a, 255
	ld (de), a 						; add a terminators

	dec de
	dec de
	dec de
	dec de
	dec de
	dec de
	dec de

	push de 
	pop iy  						; update record head position

	ret 



; ---------------------------------------------

storeTimestamp:

	MOSCALL $08						; get IX pointer to sysvars. ix is at the position we want
									; ix +0, +1 and +2

	ld a, (ix + 0)					; lowest time byte
	
	cp 255
	jr nz, @f
	ld a, 254						; don't want a time of 255 or will be an eof mark
@@:
	ld (midiTimeStamp), a 			; store the lsb time stamp

	ld a, (ix + 1)					; middle time byte
	ld (midiTimeStamp + 1), a 		; stoe the current time stamp

	ld a, (ix + 2)					; msb time byte
	ld (midiTimeStamp + 2), a 		; stoe the current time stamp

	ret 

; ---------------------------------------------

PLAY_SONG:

	ld a, 0
	ld (amRecording), a
	ld a, 1
	ld (amPlaying), a

	call resetSystime				; reset timer
	call resetRecordhead			; reset recorder
	ld iy, musicData  				; reset where data is coming from

	ld hl, playingStr
	ld bc, endPlaying - playingStr
	rst.lil $18						; print that we are loading

	ret 

; ---------------------------------------------

STOP_SONG:

	ld a, 0
	ld (amRecording), a
	ld a, 0
	ld (amPlaying), a

	call resetSystime				; reset timer
	call resetRecordhead			; reset recorder
	ld iy, musicData  				; reset where data is coming from

	ld hl, eraseStr
	ld bc, endEraseStr - eraseStr
	rst.lil $18						; print that we are loading

 	call stopAll

	ret 

; ---------------------------------------------

SAVE_SONG:

	call wait_for_release
	call STOP_RECORDING
	call getFileName

	ld hl, savingStr
	ld bc, endSaving - savingStr
	rst.lil $18						; print that we are loading

	ld de, musicData				; data to save is put here
	ld hl, fileNameBuffer 			;fileName			; the file name we are to use

	ld c, fa_create_always			; mode
	MOSCALL $0A						; MOS open file for writing

	ld c,0
	MOSCALL $0B						; MOS close all files

	ld c, fa_write					; mode
	MOSCALL $0A						; MOS open file for writing
	ld c, a  						; file handle



_saveLoop:
	ld a, (de)
	cp 255
	jr z, _doneSaving
	ld b,a 
	MOSCALL $0D						; MOS write a byte	
	inc de
	jr _saveLoop

_doneSaving:
	ld b,$ff 						; add last $ff
	MOSCALL $0D						; MOS write a byte

	ld c,0
	MOSCALL $0B						; MOS close all files



	ld hl, eraseStr
	ld bc, endEraseStr - eraseStr
	rst.lil $18					; ask for file

	ret

; ---------------------------------------------

getFileName:

	ld hl, getStr
	ld bc, endGetStr - getStr
	rst.lil $18					; ask for file

	ld a, 31
	rst.lil $10
	ld a, 0
	rst.lil $10
	ld a, 23 					; tab 0,25
	rst.lil $10

	ld hl, fileNameBuffer		; the buffer RAM
	ld e,1 						; clear before user inputs
	ld bc, 32 					; max buffer length
	MOSCALL $09

	ld hl, eraseStr
	ld bc, endEraseStr - eraseStr
	rst.lil $18					; ask for file

	call hidecursor

	ret 


; ---------------------------------------------

resetSystime:

	MOSCALL $08					; get IX pointer to sysvars

	ld a, 0
	ld (ix+0), a 
	ld (ix+1), a 
	ld (ix+2), a 
	ld (ix+3), a 


	ret 



maxData:	equ 4

; ---------------------------------------------

update_data:

	MOSCALL $08					; get IX pointer to sysvars

	push ix
	pop de 



; ---------------------------------------------

fileNameBuffer:
		.blkb 32,0

getStr:		.db 	17, 15 				; pen colour
 		.db 31,0,22,"Filename?",23,1,1
endGetStr:

eraseStr:	
	.db 31,0,22,"                                "
	.db 31,0,23,"                                "
endEraseStr:

loadingStr:
	.db 	17, 15 				; pen colour
	.db	31,0,22					; TAB to 0,0
	.db "Loading Song..."
endLoading:

savingStr:
	.db 	17, 15 				; pen colour
	.db	31,0,22					; TAB to 0,0
	.db "Saving Song..."
endSaving:

recordingStr:
	.db 	17, 15 				; pen colour
	.db	31,0,22					; TAB to 0,0
	.db "Recording Song..."
endRecording:

playingStr:
	.db 	17, 15 				; pen colour
	.db	31,0,22					; TAB to 0,0
	.db "Playing Song..."
endPlaying:

; ---------------------------------------------
;
;	Music data
;
; ---------------------------------------------

amPlaying: 		.db 0	; if playing back a song
amRecording: 	.db 0	; if recording a song

numRecords:	 	.db 0 		; number of bytes currently in uart buffer
serialBuffer:	.blkb 256,0 	; 256 bytes for uart buffer, should be plenty


; here we are going to store the music data and just run as far as memory allows



musicData: .db 1,2,3,4,5,255		; default empty file