; new keyboard routine
; list of keys used by midi
midiOcataveOffset:  equ 60 ; middle C3

keyData:    ; this will be in HL
; for each key in midi middle C 

;           map  mask        Midi   key  Note
;                76543210 
    .db     $08, 00000010b   ; 72    A   C
    .db     $04, 00000010b   ; 73    W   C#
    .db     $0A, 00000010b   ; 74    S   D
    .db     $04, 00000100b   ; 75    E   D#
    .db     $06, 00000100b   ; 76    D   E
    .db     $08, 00001000b   ; 77    F   F
    .db     $04, 00001000b   ; 78    T   F#
    .db     $0A, 00001000b   ; 79    G   G
    .db     $08, 00010000b   ; 80    Y   G#
    .db     $0A, 00010000b   ; 81    H   A
    .db     $06, 00100000b   ; 82    U   A#
    .db     $08, 00100000b   ; 83    J   B
    .db     $08, 01000000b   ; 84    K   C


keysPressed:    ; this will be in DE
; for each key, if it is pressed

;          mask  Midi   key  Note
    .db     $0   ; 72    A   C
    .db     $0   ; 73    W   C#
    .db     $0   ; 74    S   D
    .db     $0   ; 75    E   D#
    .db     $0   ; 76    D   E
    .db     $0   ; 77    F   F
    .db     $0   ; 78    T   F#
    .db     $0   ; 79    G   G
    .db     $0   ; 80    Y   G#
    .db     $0   ; 81    H   A
    .db     $0   ; 82    U   A#
    .db     $0   ; 83    J   B
    .db     $0   ; 84    K   C
    .db 255         ; marks end of list
    .db 0



checkKeyboard:
; need to check all Agon keyboard keys used for notes


; HL will be pointer to keyData
; DE will be pointer to keysPressed
; BC will use B as counter

; point ix at keyboard matrix
    MOSCALL mos_getkbmap
    

; need to loop through 13 keys
    ld bc, 0
    ld hl, keyData                      ; start of keyboard ids and masks
    ld de, keysPressed                  ; start of previous state

_keyCheckLoop:

    ld a, (hl)                          ; get key data
    ld (kIndex + 2), a                  ; set index offset we want to check

    inc hl  
    ld a, (hl)                          ; A is mask of value to check
    ld c, a                             ; C is now mask of value to check, just 1 bit set

    inc hl                              ; inc ready for next time


kIndex:
     ld a, (ix + 0)                      ; A is mask of current keyboard. DD 7E NN
                                         ; NN is set in code above

     and c                               ; mask with bitmask to check, A will now be 0 or not
     ld (keyState), a                    ; store current state

; check if changed

     ld c, a                             ; C is current state
     ld a, (de)                          ; A is previous state
     xor c                               ; see if different, if same will be 0, else a number

     jr z, _weDone                       ; skip as no change

     ld a, (keyState)                    ; A is current state
     ld (previousState), a               ; store it for next time

     cp 0                                ; 0 if not pressed, ie, key UP msg
     jr z, _notPressed

_isPressed:

    ld (de), a                          ; store it in list

    ld (previousState), a
    ld a, b                             ; this is count through list
    add a, midiOcataveOffset            ; convert to midi number
    ld (MidiNumber), a                  ; store for simulated note press
    ld (midiCommand + 1), a             ; store for simulated note press

    ld a, 127
    ld (MidiValue), a                   ; store value of strike, make max
    ld (midiCommand + 2), a             ; store value of strike, make max


    ld a, 1
    ld (midi_received), a               ; flag set that new key press received

    ret                                 ; if new, then wait for next time to catch any others

    ;jr _weDone


_notPressed:


    ld (previousState), a

    ld a, (previousState)


    ld (de), a                          ; store it in list

    ld a, b                             ; this is count through list
    add a, midiOcataveOffset            ; convert to midi number
    ld (MidiNumber), a                  ; store for simulated note press
    ld (midiCommand + 1), a             ; store for simulated note press

    ld a, 0
    ld (MidiValue), a                   ; store value of strike, make max
    ld (midiCommand + 2), a             ; store value of strike, make max


    ld a, 1
    ld (midi_received), a               ; flag set that new key press received

    ret                                 ; if new, then wait for next time to catch any others


_weDone:
    inc de                              ; inc for next check

    inc b                               ; increase loop counter
    ld a, b                             ; put count in A

    cp 13                               ; have we looped 13 times?
    jp nz, _keyCheckLoop                ; if not loop round again


    ret 


; at the end set midi_received to 1 or 0
; if 1, then also fill  midiNumber, midiValue
; main routine will then deal with midi same as serial input


previousState:  .db 0
keyState:       .db 0 

keyIndex:       .db 0
keyMask:        .db 0
keyStatus:      .db 0


