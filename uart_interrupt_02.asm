; trying to figure out a UART interruopt handler

;	UART CONSTANTS - FOR REFERENCE, NOT ALL ARE USED

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

; ---------------------------------------------
;
;	INITIALISE
;
; ---------------------------------------------

	.assume adl=1				; big memory mode
	.org $40000				; load code here

	jp start_here				; jump to start of code

	.align 64				; MOS header
	.db "MOS",0,1

; ---------------------------------------------
;
;	INITIAL SETUP CODE HERE
;
; ---------------------------------------------

start_here:
						; store everything as good practice	
	push af					; pop back when we return from code later
	push bc
	push de
	push ix
	push iy

	im 2

	ld a, 0
	ld (had_data),a

	call CLS
	call openUART1				; open serial port for comms

; ---------------------------------------------
;
;	MAIN LOOP
;
; ---------------------------------------------

MAIN_LOOP:	
	MOSCALL $08					; get IX pointer to sysvars
	ld a, (ix + 05h)				; ix+5h is 'last key pressed'
	cp 27						; is it ESC key?
	jp z, exit_here					; if so exit cleanly

	call display_buffer

	jr MAIN_LOOP

; ---------------------------------------------

display_buffer:						; display the buffer contents

	TABTO 0, 2   					; set start position

	ld a, (had_data)
 	call printDec 

	ld hl, LINEFEED					; pointer to newline char sequence
	call printString				; print a new line

	ld de, uart_buffer				; HL is start of buffer bytes
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

	call closeUART1
	call CLS
	
	pop iy
	pop ix
	pop de
	pop bc
	pop af
	ld hl,0

	ret						; return to MOS here


; ---------------------------------------------
;
;	UART ROUTINES
;
; ---------------------------------------------

openUART1:
	ld ix, UART1_Struct
	MOSCALL $15					; open uart1

   	ld hl, uart_isr_handler
	ld e, $1A        				; interrupt number (UART1)
	ld a, $14     					; mos_api_setintvector
	rst.lil $08

; mos_api_uopen:        
; 	LEA    HL, IX + 0    ; HLU: Pointer to struct

	ld hl, uart_buffer
	ld (uart_buf_pos), hl				; get curent buffer write position


	ret 

; ---------------------------------------------

closeUART1:
	MOSCALL $16 					; close uart1
	ret 

; ---------------------------------------------

UART1_Struct:	
	.dl 	31250; baud_rate			; baud rate (stored as three byte LONG)
	.db 	8 					; data bits
	.db 	1 					; stop bits
	.db 	0 					; parity bits
	.db 	0					; flow control
	.db 	00000001b				; interrupt bits - bit 0 is enable received data interrupt

							;    - Bit 0: Set to enable received data interrupt
							;    - Bit 1: Set to enable transmit data interrupt
							;    - Bit 2: Set to enable line status change interrupt
							;    - Bit 3: Set to enable modem status change interrupt
							;    - Bit 4: Set to enable transmit complete interrupt

; ---------------------------------------------

uart_isr_handler:			; this is the interrupt service routine (ISR) that gets called
	di				; stop any other interrupts while we do this
	push af
	push hl 			; store any registers we are going to use

	; do ISR code here

	in0 a,(REG_RBR)			; read byte from buffer

	ld hl,(uart_buf_pos)		; get curent buffer write position
	ld (hl), a 			; store byte received
	inc hl				; inc for next time
	ld (uart_buf_pos), hl  		; store pointer to buffer position

	ld a, (had_data)
	inc a
	ld (had_data),a  		; just counting the bytes coming in

	pop hl
	pop af				; return register values
	ei				; enable interrupts again

	reti.l  			; return from ISR

had_data: 	.db 0			; count of bytes received, just for testing

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

; ---------------------------------------------

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

; ---------------------------------------------

printString:                ; print zero terminated string
    ld a,(hl)
    or a
    ret z
    RST.LIL 10h
    inc hl
    jr printString


; ---------------------------------------------

LINEFEED:           .asciz "\r\n"

uart_buffer:				; 256 byte buffer
	.db	"                                                              "

; ---------------------------------------------
;
;	END
;
; ---------------------------------------------



;    ld hl, uart0_rx_interrupt
;     ld e, 0x18        ; interrupt number
;     ld a, 0x14     ; mos_api_setintvector
;     rst.lil 8




