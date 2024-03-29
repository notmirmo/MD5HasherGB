include "hardware.inc"

SECTION "Header", ROM0[$100]
	
	jp EntryPoint

	ds $150 - @, 0 ; Make Room for the header

EntryPoint:
	; Do not turn the LCD off outside of VBlank
WaitVBlank:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank

	; turn the LCD off
	ld a, 0
	ld [rLCDC], a

	ld b, 160
	ld hl, _OAMRAM
ClearOam:
	ld [hli], a
	dec b
	jp nz, ClearOam

	;Copy the K constant in Work RAM ; stocker de 0xC010 à 0xC10F
	ld de, ConstVarK
	ld hl, $C010
	ld bc, ConstVarKEnd - ConstVarK
	call Memcopy

	;Copy the H constant in Work RAM ; stocker à $C110
	ld de, ConstVarH
	ld hl, $C110
	ld bc, ConstVarHEnd - ConstVarH
	call Memcopy

	;Copy the R constant in Work RAM ; stocker à $C200
	ld de, ConstVarR
	ld hl, $C200
	ld bc, ConstVarREnd - ConstVarR
	call Memcopy

	; Copy Tile Data
	ld de, Tiles
	ld hl, $9000
	ld bc, TilesEnd - Tiles ; exemple : TE:230, Ti:100 230-100 = 130 donc 130 Tiles a copier en mémoire
	call Memcopy

	;Copy the tilemap
	ld de, Tilemap
	ld hl, $9800
	ld bc, TilemapEnd - Tilemap
	call Memcopy

	ld de,TilesObj
	ld hl,$8000
	ld bc, TilesObjEnd - TilesObj
	call Memcopy

	ld hl, _OAMRAM
	ld a, 16 + 104 ; y = 104
	ld [hli],a
	ld a, 8 + 24   ; x = 24
	ld [hli], a
	ld a, 0
	ld [hli], a
	ld [hli], a

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
	ld [rLCDC], a

	; During the first (blank) frame, initalize display registers
	ld a, %11100100
	ld [rBGP], a
	ld a, %11100100
	ld [rOBP0], a

  ld a, 0
  ld [wCurKeys], a
  ld [wNewKeys], a
  ld [wLastKey], a
  ld [wLetterPos], a
  ld [wLetterPosMem], a
  ld [varNumBlock], a
  ld [varNumBlock2], a
  ld [wOutputPos], a

  ;initialize ascii str at 00
  ld hl,$C120
  ld [hl], $00


Main:
	; Wait until it's not VBlank
	ld a, [rLY]
	cp 144
	jp nc, Main
WaitVBlank2:
	ld a,[rLY]
	cp 144
	jp c, WaitVBlank2

	call UpdateKeys
	call UnpressKey

CheckA:
	ld a, [wCurKeys]
	and a, PADF_A
	jp z, CheckLeft
	ld a, [wLastKey]
	and a, PADF_A
	jp nz, Main

	ld a, PADF_A
	ld [wLastKey],a

	ld a,[_OAMRAM+1]
	sub 8
	ld b,a
	ld a,[_OAMRAM]
	sub 16
	ld c,a
	call GetTileByPixel
	call PlaceLetter
	call MD5Algo
	jp Main

CheckLeft:
	ld a, [wCurKeys]
	and a, PADF_LEFT
	jp z, CheckRight
	ld a, [wLastKey]
	and a, PADF_LEFT
	jp nz, Main
Left:
	ld a, PADF_LEFT
	ld [wLastKey],a
	; Move to 1 pixel to left
	ld a, [_OAMRAM +1]
	sub 8
	
	cp a, 24
	jp z, Main
	ld [_OAMRAM +1], a
	jp Main

CheckRight:
	ld a, [wCurKeys]
	and a, PADF_RIGHT
	jp z, CheckUp
	ld a, [wLastKey]
	and a, PADF_RIGHT
	jp nz, Main
Right:
	ld a, PADF_RIGHT
	ld [wLastKey],a
	; Move to 1 pixel to left
	ld a, [_OAMRAM +1]
	add 8
	ld b,a
	cp a, 120
	jp z, Main
	ld a,[_OAMRAM]
	cp a, 104 + 8 + 8
	jp z, Right2
	cp a, 104 + 16 + 8
	jp z, Right2
	ld a,b
	cp a, 72+8
	jp nc, Main
Right2:
	ld a,b
	ld [_OAMRAM +1], a
	jp Main


CheckUp:
	ld a, [wCurKeys]
	and a, PADF_UP
	jp z, CheckDown
	ld a, [wLastKey]
	and a, PADF_UP
	jp nz, Main
Up:
	ld a, PADF_UP
	ld [wLastKey],a
	; Move to 1 pixel to left
	ld a, [_OAMRAM]
	sub 8
	
	cp a, 96 + 16
	jp z, Main
	ld [_OAMRAM], a
	jp Main

CheckDown:
	ld a, [wCurKeys]
	and a, PADF_DOWN
	jp z, Main
	ld a, [wLastKey]
	and a, PADF_DOWN
	jp nz, Main
Down:
	ld a, PADF_DOWN
	ld [wLastKey],a
	; Move to 1 pixel to left
	ld a, [_OAMRAM]
	add 8
	
	cp a, 128 + 16
	jp z, Main
	cp a, 104 + 16 + 8
	jp z, Down2
	ld b,a
	ld a,[_OAMRAM + 1]
	cp 72+8
	jp nc, Main
	ld a,b
Down2:
	ld [_OAMRAM], a
	jp Main


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                 MD5 ALGORITHME                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mem Location:                                      ;
;  0xC010 à 0xC10F -> K Constant                     ;
;  0xC200 à 0xC23F -> R Constant                     ;
;                                                    ;
;  0xC110 à 0xC113 -> h0                             ;
;  0xC114 à 0xC117 -> h1                             ;
;  0xC118 à 0xC11B -> h2                             ;
;  0xC11C à 0xC11F -> h3                             ;
;                                                    ;
;  0xC120          -> the ascii to be hashed         ;
;  0xC160          -> Message prep                   ;
;                                                    ;
;  0xC1E0 à 0xC1E3 -> A                              ;
;  0xC1E4 à 0xC1E7 -> B                              ;
;  0xC1E8 à 0xC1EB -> C                              ;
;  0xC1EC à 0xC1EF -> D                              ;
;                                                    ;
;  0xC1F0 à 0xC1F3 -> F                              ;
;  0xC1F4 à 0xC1F7 -> G                              ;
;                                                    ;
;  0xC1F8 à 0xC1FB -> Temp var 1                     ;
;  0xC1FC à 0xC1FF -> Temp var 2                     ;
;                                                    ;
;  0xC240 à 0xC24F -> MD5 Hash                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


MD5Algo:
	call MD5ResetValue
	call MD5Padding
	call MD5Message
	call MD5Output
	ret

MD5Padding:
	ld hl, $C160
	ld bc, $C120
	ld de,0
CopyAscii:
	ld a,[bc]
	cp $00
	jp z , CopyAsciiEnd
	ld [hli],a
	inc bc
	ld a,8
	add a,e
	ld e,a
	ld a,d
	adc a,0 ; PK CA BUG
	ld d,a
	jp CopyAscii
CopyAsciiEnd: ; Now de = len(msg) in bit
	
	ld [hl], $80 ; append 1 bit at then end of msg
	inc hl
	push hl

	ld a,d
	cp $01
	jp c, MessageLess ; jmp if d<$01  ;
	ld a,e                            ;
	cp $B9                            ;
	jp c, MessageLess ; jmp if e<$B9  ; jmp if de<$01B8 
MessageMore:
	ld bc,$01C0

	ld a,e
	sub c
	ld c,a

	ld a,d
	sbc b
	ld b,a; now bc = de-448

	ld hl,$0200

	ld a,l
	sub c  ; sub l,c ;
	ld c,a           ;
                     ;
	ld a,h           ;
	sbc b  ;sub h,b  ; sub hl, bc
	ld b,a           ; normaly now we have bc = (512 - (msg-448)) ; MARCHE PAS A FINIR

	ld a,2             ;
	ld [varNumBlock],a ; varNumBlock = 2

	jp MessageLenEnd
MessageLess:
	ld bc,$01C0

	ld a,c
	sub e ; sub c,e
	ld c,a

	ld a,b
	sbc d
	ld b,a; now bc = 448-de

	ld a,1             ;
	ld [varNumBlock],a ; varNumBlock = 1

MessageLenEnd: ; normaly bc = the nb of '0'  to append to the msg
	pop hl     ; Now we append bc '0' to the message
	srl b          ;
	ld a,%01111111 ;
	adc 0          ;
	and %10000000  ;
	srl c          ;
	or c           ;
	ld c, a        ; Divise par 2
	srl b          ;
	ld a,%01111111 ;
	adc 0          ;
	and %10000000  ;
	srl c          ;
	or c           ;
	ld c, a        ; Divise par 4
	srl b          ;
	ld a,%01111111 ;
	adc 0          ;
	and %10000000  ;
	srl c          ;
	or c           ;
	ld c, a        ; Divise par 8 | bc = bc/8

	dec c ; on retire le 0x80 déjà mis avant

Padding0:
	ld a,c
	cp 0
	jp z, Padding0End
	ld [hl], $00
	inc hl
	dec c
	jp Padding0
Padding0End:

	ld [hl], e ;
	inc hl     ;
	ld [hl], d ;
	inc hl     ; on entre la taille en little-endian

	ld a,6
PaddingLen:
	ld [hl], $00
	inc hl
	dec a
	cp 0
	jp nz, PaddingLen

	ret



MD5Message:
	
	ld a,[varNumBlock]
	cp 0
	jp z,MD5MessageEnd
	

	ld hl, $C110 ; h0
	ld de, $C1E0 ; A
	call ld32bit ; A = h0

	ld hl, $C114 ; h1
	ld de, $C1E4 ; B
	call ld32bit ; B = h1

	ld hl, $C118 ; h2
	ld de, $C1E8 ; C
	call ld32bit ; C = h2

	ld hl, $C11C ; h3
	ld de, $C1EC ; D
	call ld32bit ; D = h3

	ld a,0
	ld [varI],a
ForIFrom0to64:
	ld a,[varI]
	cp 64
	jp z, BlockEnd
	cp 16
	jp c, I0to15 ; saute if 0 ≤ a ≤ 15
	cp 32
	jp c, I16to31 ; saute if 16 ≤ a ≤ 31
	cp 48
	jp c, I32to47 ; saute if 32 ≤ a ≤ 47
	cp 64
	jp c, I48to63 ; saute if 48 ≤ a ≤ 63
I0to15: ; if 0 ≤ a ≤ 15
	
	call FunctionF1
	call FunctionG1
	jp AfterCondtion
I16to31: ; if 16 ≤ a ≤ 31
	
	call FunctionF2
	call FunctionG2
	jp AfterCondtion
I32to47: ; 32 ≤ a ≤ 47

	call FunctionF3
	call FunctionG3
	jp AfterCondtion
I48to63: ; 48 ≤ a ≤ 63

	call FunctionF4
	call FunctionG4
	jp AfterCondtion
AfterCondtion:

	ld hl, $C1EC ; D
	ld de, $C1F8 ; Temp1
	call ld32bit ; Temp1 = D

	ld hl, $C1E8 ; C
	ld de, $C1EC ; D
	call ld32bit ; D = C

	ld hl, $C1E4 ; B
	ld de, $C1E8 ; C
	call ld32bit ; C = B

	;HERE WE DO THE b = leftrotate((a + f + k[i] + w[g]), r[i]) + b
	ld hl, $C1E0 ; A
	ld de, $C1FC ; Temp2
	call ld32bit ; Temp2 = A

	ld hl, $C1F0 ; F
	ld de, $C1FC ; Temp2
	call add32bitmodulo ; Temp2 = Temp2 + F ; Temps2 = A + F

	ld hl, $C010 ; K
	ld a, [varI]  ;
	sla a ; a * 2 ;
	sla a ; a * 4 ;
	ld b,0        ;
	ld c,a        ;
	add hl, bc    ;Now hl = K[i*4] (we need to go 4 by 4 because K is a 32 bit var array and not 8bit)
	ld de, $C1FC ; Temp2
	call add32bitmodulo ; Temp2 = Temp2 + K[i*4] ; Temps 2 = A + F + K[i*4]

	ld a,[varNumBlock2] ; pb quand var = 2, il prend C1A0 a traiter en premier alors que ça doit être l'inverse
	cp 1
	jp z, BlockE2
	ld hl, $C160 ; Message Prep 1
	jp BlockE2End
BlockE2:
	ld hl, $C1A0 ; Message Prep 2
BlockE2End:
	ld a, [$C1F7]  ; LSB of G
	sla a ; a * 2 ;
	sla a ; a * 4 ;
	ld b, 0       ;
	ld c, a       ;
	add hl, bc    ; Now hl = w[g*4] (we need to go 4 by 4 because The message is separate in 16 32 bit word)
	ld de, $C1F4 ; G
	call littleEndianLd32bit
	ld hl,$C1F4
	ld de, $C1FC ; Temp2
	call add32bitmodulo ; Temp2 = Temp2 + w[g*4] ; Temp2 = A + F + K[i*4] + w[g*4]

	ld hl, $C200 ;
	ld a, [varI] ;
	ld b,0       ;
	ld c,a       ;
	add hl, bc   ;
	ld b, [hl]   ; ld b, r[i]
	ld hl, $C1FC ; Temp2
	call rl32bit ; leftrotate(Temp2,r[i])

	ld hl, $C1FC ; Temp2
	ld de, $C1E4 ; B
	call add32bitmodulo ; B = leftrotate(Temp2,r[i]) + B
	; The leftrotate is done.


	ld hl, $C1F8 ; Temp1
	ld de, $C1E0 ; A
	call ld32bit ; A = Temp1

	ld a,[varI]
	inc a
	ld [varI],a

	jp ForIFrom0to64
BlockEnd:
	ld hl, $C1E0 ; A
	ld de, $C110 ; h0
	call add32bitmodulo ; h0 = h0 + A

	ld hl, $C1E4 ; B
	ld de, $C114 ; h1
	call add32bitmodulo ; h1 = h1 + B
	
	ld hl, $C1E8 ; C
	ld de, $C118 ; h2
	call add32bitmodulo ; h2 = h2 + C
	
	ld hl, $C1EC ; D
	ld de, $C11C ; h3
	call add32bitmodulo ; h3 = h3 + D

	ld hl, $C110 ; A
	ld de, $C240 ; 
	call littleEndianLd32bit

	ld hl, $C114 ; B
	ld de, $C244 ; 
	call littleEndianLd32bit

	ld hl, $C118 ; C
	ld de, $C248 ; 
	call littleEndianLd32bit

	ld hl, $C11C ; D
	ld de, $C24C ; 
	call littleEndianLd32bit

	ld a,[varNumBlock]
	dec a
	ld [varNumBlock],a
	ld a,[varNumBlock2]
	inc a
	ld [varNumBlock2],a
	jp MD5Message
	
MD5MessageEnd:
	ret

MD5Output:

	ld hl, $C240
	ld de, $9901
MD5OutputLoop:
	ld a,e
	cp $0F
	jp z, MD5OutputPlus
	cp $2F
	jp z, MD5OutputPlus
	jp MD5OutputPlusEnd
MD5OutputPlus:
	add 18
	ld e,a
MD5OutputPlusEnd:

	ld a, [hl]
	ld b,a
	and %11110000
	srl a
	srl a
	srl a
	srl a
	cp $0A
	call nc, OutputLetter
	call c, OutputNumber
	ld a,b
	and %00001111
	cp $0A
	call nc, OutputLetter
	call c, OutputNumber
	inc hl
	ld a,[wOutputPos]
	cp 32
	jp nz,MD5OutputLoop
	ret

OutputLetter:
	sub 9
	ld c,a
	call WaitVBlankF
	ld a,c
	ld [de],a
	ld a,[wOutputPos]
	inc a
	ld [wOutputPos],a
	inc de
	ret
OutputNumber:
	add 27
	ld c,a
	call WaitVBlankF
	ld a,c
	ld [de],a
	ld a,[wOutputPos]
	inc a
	ld [wOutputPos],a
	inc de
	ret



MD5ResetValue:

	ld a,0
	ld hl, $C110 ; A
	ld [hli], a ;
	ld [hli], a ;
	ld [hli], a ;
	ld [hli], a ; Put A to 0

	ld hl, $C110 ; A
	ld hl, $C114 ; B
	call ld32bit ; B = A

	ld hl, $C110 ; A
	ld hl, $C118 ; C
	call ld32bit ; C = A

	ld hl, $C110 ; A
	ld hl, $C11C ; D
	call ld32bit ; D = A

	;Reset Hx
	ld de, ConstVarH
	ld hl, $C110
	ld bc, ConstVarHEnd - ConstVarH
	call Memcopy

	ld a,0
	ld [varNumBlock2], a
	ld [wOutputPos], a

	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                END OF MD5 ALGORITHME               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                 FUNCTION SECTION                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FunctionF1: ; F = (B and C) or ((not B) and D)  // $C1F8 var temp 1 et $C1FC var temp 2
	ld hl, $C1E4 ; B                     ;                                      ;
	ld de, $C1F8 ; Temp1                 ;                                      ;
	call ld32bit ; move B dans Temp1     ;                                      ;
                                         ;                                      ;
	ld hl, $C1E8  ; C                    ;                                      ;
	ld de, $C1F8  ; Temp1                ;                                      ;
	call and32bit ; Temp1 = Temp1 and C  ; Temps1 = (B and C)                   ;
                                                                                ;
	ld hl, $C1E4 ; B                   ;                                        ;
	ld de, $C1FC ; Temp2               ;                                        ;
	call ld32bit ; move B dans Temp2   ;                                        ;
	                                   ;                                        ;
	ld hl, $C1FC  ; Temp2              ;                                        ;
	call not32bit ; Temp2 = not(Temp2) ; Temp2 = not(B)                         ;
                                                                                ;
	ld hl,$C1EC   ; D                   ;                                       ;
	ld de,$C1FC   ; Temp2               ;                                       ;
	call and32bit ; Temp2 = Temp2 and D ; Temp2 = (not(B) and D)                ;
                                                                                ;
	ld hl, $C1F8 ; Temp1                  ;                                     ;
	ld de, $C1FC ; Temp2                  ;                                     ;
	call or32bit ; Temp2 = Temp1 or Temp2 ; Temp2 = (B and C) or (not(B) and D) ;
                                                                                ;
	ld hl, $C1FC ; Temp2                                                        ;
	ld de, $C1F0 ; F                                                            ;
	call ld32bit ; move Temp2 dans F =========================================> ; F = (B and C) or ((not B) and D)

	ret

FunctionF2: ; F = (D and B) or ((not D) and C)
	ld hl, $C1EC ; D                     ;                                      ;
	ld de, $C1F8 ; Temp1                 ;                                      ;
	call ld32bit ; move D dans Temp1     ;                                      ;
                                         ;                                      ;
	ld hl, $C1E4  ; B                    ;                                      ;
	ld de, $C1F8  ; Temp1                ;                                      ;
	call and32bit ; Temp1 = Temp1 and B  ; Temps1 = (D and B)                   ;
                                                                                ;
	ld hl, $C1EC ; D                   ;                                        ;
	ld de, $C1FC ; Temp2               ;                                        ;
	call ld32bit ; move D dans Temp2   ;                                        ;
	                                   ;                                        ;
	ld hl, $C1FC  ; Temp2              ;                                        ;
	call not32bit ; Temp2 = not(Temp2) ; Temp2 = not(D)                         ;
                                                                                ;
	ld hl,$C1E8   ; C                   ;                                       ;
	ld de,$C1FC   ; Temp2               ;                                       ;
	call and32bit ; Temp2 = Temp2 and C ; Temp2 = (not(D) and C)                ;
                                                                                ;
	ld hl, $C1F8 ; Temp1                  ;                                     ;
	ld de, $C1FC ; Temp2                  ;                                     ;
	call or32bit ; Temp2 = Temp1 or Temp2 ; Temp2 = (B and C) or (not(B) and D) ;
                                                                                ;
	ld hl, $C1FC ; Temp2                                                        ;
	ld de, $C1F0 ; F                                                            ;
	call ld32bit ; move Temp2 dans F =========================================> ; F = (D and B) or ((not D) and C)
	
	ret

FunctionF3: ; F = B xor C xor D
	ld hl,$C1E4  ; B            ;
	ld de,$C1F0  ; F            ;
	call ld32bit ; F = B        ;
                                ;
	ld hl,$C1E8   ; C           ;
	ld de,$C1F0   ; F           ;
	call xor32bit ; F = F xor C ;
                                ;
	ld hl,$C1EC  ; D            ;
	ld de,$C1F0  ; F            ;
	call xor32bit ; F = F xor D ; F = B xor C xor D
	
	ret

FunctionF4: ; F = C xor (B or (not D))
	ld hl, $C1EC ; D
	ld de, $C1F0 ; F
	call ld32bit ; F = D

	ld hl, $C1F0  ; F          ;
	call not32bit ; F = not(F) ; F = not(D)

	ld hl, $C1E4 ; B          ;
	ld de, $C1F0 ; F          ;
	call or32bit ; F = B or F ; F = B or not(D)

	ld hl, $C1E8  ; C           ;
	ld de, $C1F0  ; F           ;
	call xor32bit ; F = C xor F ; F = C xor (B or not(D))

	ret

FunctionG1: ; G = i
	ld hl, $C1F4 ; G
	ld [hl], $00 ; G(H-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(L-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(H-LSB) = 00
	inc hl       ;
	ld a,[varI]  ;
	ld [hl],a    ; G(L-LSB) = varI
	ret

FunctionG2: ; G = (5×i + 1) mod 16
	ld hl, $C1F4 ; G
	ld [hl], $00 ; G(H-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(L-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(H-LSB) = 00
	inc hl

	ld [hl],$05 ; hl = 5
	ld a,[varI] ;
	ld d,a      ; d = varI
	call mult8bit ; [hl] = 5 * i
	inc [hl] ; G + 1
	ld a, %00001111 ;
	and [hl]        ;
	ld [hl],a       ; G mod 16

	ret

FunctionG3: ; G = (3×i + 5) mod 16
	ld hl, $C1F4 ; G
	ld [hl], $00 ; G(H-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(L-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(H-LSB) = 00
	inc hl

	ld [hl],$03 ; hl = 3
	ld a,[varI] ;
	ld d,a      ; d = varI
	call mult8bit ; [hl] = 3 * i
	ld a,[hl] ;
	add 5     ;
	ld [hl],a ; G + 5
	ld a, %00001111 ;
	and [hl]        ;
	ld [hl],a       ; G mod 16
	
	ret

FunctionG4: ; G = (7×i) mod 16
	ld hl, $C1F4 ; G
	ld [hl], $00 ; G(H-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(L-HSB) = 00
	inc hl       ;
	ld [hl], $00 ; G(H-LSB) = 00
	inc hl

	ld [hl],$07 ; hl = 7
	ld a,[varI] ;
	ld d,a      ; d = varI
	call mult8bit ; [hl] = 7 * i
	ld a, %00001111 ;
	and [hl]        ;
	ld [hl],a       ; G mod 16
	
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                32 bit sized function               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;move hl into de but in little endian
littleEndianLd32bit:
	inc hl
	inc hl
	inc hl
	
	ld a,[hl]
	ld [de], a
	inc de
	dec hl

	ld a,[hl]
	ld [de], a
	inc de
	dec hl

	ld a,[hl]
	ld [de], a
	inc de
	dec hl

	ld a,[hl]
	ld [de], a

	ret

;move hl into de
ld32bit:
	ld a,[hl]
	ld [de],a
	inc de
	inc hl
	ld a,[hl]
	ld [de],a
	inc de
	inc hl
	ld a,[hl]
	ld [de],a
	inc de
	inc hl
	ld a,[hl]
	ld [de],a
	inc de
	inc hl
	ret

;@param b : num of time to rotate.
;@param hl : location of the 32 bit var
rl32bit:
	push hl
	ld a,[hl]
	and %10000000
	add %11111111 ; now normaly the carry flag is equal to HSB of 'a'
	inc hl
	inc hl
	inc hl
	rl [hl]
	dec hl
	rl [hl]
	dec hl
	rl [hl]
	dec hl
	rl [hl]

	pop hl
	dec b
	ld a,b
	cp 0
	jp nz, rl32bit
	ret

;@param hl : location of var 1
;@param de : location of var 2
; return de = (hl + de) mod 32bits
add32bitmodulo:
	inc hl
	inc hl
	inc hl
	inc de
	inc de
	inc de

	ld a,[de] ;
	add [hl]  ;
	ld [de],a ;
	dec hl    ;
	dec de    ; add Low LSB

	ld a,[de] ;
	adc [hl]  ;
	ld [de],a ;
	dec hl    ;
	dec de    ; add High LSB

	ld a,[de] ;
	adc [hl]  ;
	ld [de],a ;
	dec hl    ;
	dec de    ; add Low HSB

	ld a,[de] ;
	adc [hl]  ;
	ld [de],a ; add High HSB

	ret

;@param hl : loc of var 1
;@param de : loc of var 2
;return de = hl AND de
and32bit:
	ld a,[de] ;
	and [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; and High HSB
	ld a,[de] ;
	and [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; and Low HSB
	ld a,[de] ;
	and [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; and High LSB
	ld a,[de] ;
	and [hl]  ;
	ld [de],a ; and Low LSB

	ret

;@param hl : loc of var 1
;@param de : loc of var 2
;return de = hl xor de
xor32bit:
	ld a,[de] ;
	xor [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; xor High HSB
	ld a,[de] ;
	xor [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; xor Low HSB
	ld a,[de] ;
	xor [hl]  ;
	ld [de],a ;
	inc hl    ;
	inc de    ; xor High LSB
	ld a,[de] ;
	xor [hl]  ;
	ld [de],a ; xor Low LSB

	ret

;@param hl : loc of var 1
;@param de : loc of var 2
;return de = hl OR de
or32bit:
	ld a,[de] ;
	or [hl]   ;
	ld [de],a ;
	inc hl    ;
	inc de    ; or Hish HSB
	ld a,[de] ;
	or [hl]   ;
	ld [de],a ;
	inc hl    ;
	inc de    ; or Low HSB
	ld a,[de] ;
	or [hl]   ;
	ld [de],a ;
	inc hl    ;
	inc de    ; or High LSB
	ld a,[de] ;
	or [hl]   ;
	ld [de],a ; or Low LSB

	ret
;@param hl : loc of var 1
;return hl = !hl
not32bit:
	ld a,$FF  ;
	xor [hl]  ;
	ld [hl],a ;
	inc hl    ; not High HSB
	ld a,$FF  ;
	xor [hl]  ;
	ld [hl],a ;
	inc hl    ; not Low HSB
	ld a,$FF  ;
	xor [hl]  ;
	ld [hl],a ;
	inc hl    ; not High LSB
	ld a,$FF  ;
	xor [hl]  ;
	ld [hl],a ; not Low LSB

	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            end of 32 bit sized function            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mult in 8 bit : hl = hl * d  ; hl = 2 et d = 3
mult8bit:
	ld a,[hl] ; a = 8
	ld b,a    ; b = 2
	dec d
mult8bitContinue:
	add b 
	dec d
	jp nz, mult8bitContinue
	ld [hl],a
	ret


; param d : letter tile
PlaceLetterInMem:
	ld hl, $C120
	ld a, [wLetterPosMem]
	ld b,0
	ld c,a
	add hl,bc
	inc a
	ld [wLetterPosMem],a

	ld a,d
	cp $2F
	jp z, IsSpace  ; Jump if a == $2F
	cp $1B
	jp nc,IsNumber ; Jump if a > $1B
IsChar:
	add a, $60
	jp WriteASCII
IsNumber:
	add $15
	jp WriteASCII
IsSpace:
	ld a,$20
WriteASCII:
	ld [hli], a
	ld [hl], $00
	ret


EraseLetterInMem:
	ld hl, $C120
	ld a, [wLetterPosMem]
	dec a
	ld b,0
	ld c,a
	add hl,bc
	ld [hl],$00
	ld [wLetterPosMem],a
	ret

WaitVBlankF:
	ld a,[rLY]
	cp 144
	jp c, WaitVBlankF
	ret

;Copy byte form one area to another
; de = Source
; hl = Destination
; bc = Length
Memcopy:
	ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, Memcopy
    ret

UnpressKey:
	ld a,[wCurKeys]
	cp 0
	ret nz
	ld [wLastKey],a
	ret


PlaceLetter: ;$984F
	ld b,[hl]
	ld a,b
	cp $2D
	jp z, EraseLeter ;check if its "<-" and jump to another section. 
	ld hl, $9841
	ld a, [wLetterPos]
	cp 14
	jp nz, PlaceLetter1
	add 18
PlaceLetter1:
	cp 46
	jp nz, PlaceLetter2
	add 18
PlaceLetter2:
	cp 78
	jp nz, PlaceLetter3
	add 18
PlaceLetter3:
	cp 110
	jp z,Main

	ld d,0
	ld e,a
	add hl, de
	inc a
	ld [wLetterPos],a
	ld [hl], b

	ld d,b
	call PlaceLetterInMem
	ret


;Erase Part;
EraseLeter:
	ld hl, $9841
	ld a, [wLetterPos]
	dec a
	cp 31
	jp nz, EraseLetter1
	sub 18
EraseLetter1:
	cp 63
	jp nz, EraseLetter2
	sub 18
EraseLetter2:
	cp 95
	jp nz, EraseLetter3
	sub 18
EraseLetter3:
	inc a
	cp 0
	jp z,Main
	dec a

	ld [wLetterPos],a
	ld d,0
	ld e,a
	add hl, de
	ld [hl], $2F
	call EraseLetterInMem
	ret


; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:
    ; First, we need to divide by 8 to convert a pixel position to a tile position.
    ; After this we want to multiply the Y position by 32.
    ; These operations effectively cancel out so we only need to mask the Y value.
    ld a, c
    and a, %11111000
    ld l, a
    ld h, 0
    ; Now we have the position * 8 in hl
    add hl, hl ; position * 16
    add hl, hl ; position * 32
    ; Convert the X position to an offset.
    ld a, b
    srl a ; a / 2
    srl a ; a / 4
    srl a ; a / 8
    ; Add the two offsets together.
    add a, l
    ld l, a
    adc a, h
    sub a, l
    ld h, a
    ; Add the offset to the tilemap's base address, and we are done!
    ld bc, $9800
    add hl, bc
    ret

UpdateKeys:
  ; Poll half the controller
  ld a, P1F_GET_BTN
  call .onenibble
  ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a, P1F_GET_DPAD
  call .onenibble
  swap a ; A3-0 = unpressed directions; A7-4 = 1
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; And release the controller
  ld a, P1F_GET_NONE
  ldh [rP1], a

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret

.onenibble
  ldh [rP1], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
  ldh a, [rP1]
  ldh a, [rP1] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                                           ;
;                                                 Variable and Graphisme                                                    ;
;                                                        SECTION                                                            ;
;                                                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



SECTION "Tile data", ROM0

Tiles:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; Blank (White) $00
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $80, $7E, $84, $42, $84, $42 ; A $01 // Letters
	db $00, $00, $80, $7C, $84, $42, $84, $42, $80, $7C, $84, $42, $84, $42, $80, $7C ; B $02
	db $00, $00, $40, $3C, $84, $42, $80, $40, $80, $40, $80, $40, $84, $42, $40, $3C ; C $03
	db $00, $00, $80, $7C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $80, $7C ; D $04
	db $00, $00, $80, $7C, $80, $40, $80, $40, $80, $7C, $80, $40, $80, $40, $80, $7C ; E $05
	db $00, $00, $80, $7C, $80, $40, $80, $40, $80, $7C, $80, $40, $80, $40, $80, $40 ; F $06
	db $00, $00, $40, $38, $88, $44, $80, $40, $80, $40, $90, $4E, $88, $44, $40, $3C ; G $07
	db $00, $00, $84, $42, $84, $42, $84, $42, $80, $7E, $84, $42, $84, $42, $84, $42 ; H $08
	db $00, $00, $40, $3E, $10, $08, $10, $08, $10, $08, $10, $08, $10, $08, $40, $3E ; I $09
	db $00, $00, $40, $3E, $10, $08, $10, $08, $10, $08, $90, $48, $90, $48, $40, $30 ; J $0A
	db $00, $00, $88, $44, $90, $48, $A0, $50, $80, $60, $A0, $50, $90, $48, $88, $44 ; K $0B
	db $00, $00, $40, $20, $40, $20, $40, $20, $40, $20, $40, $20, $40, $20, $40, $3C ; L $0C
	db $00, $00, $88, $44, $90, $6C, $A8, $54, $88, $44, $88, $44, $88, $44, $88, $44 ; M $0D
	db $00, $00, $84, $42, $84, $62, $A4, $52, $94, $4A, $88, $46, $84, $42, $84, $42 ; N $0E
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; O $0F
	db $00, $00, $80, $78, $88, $44, $88, $44, $80, $78, $80, $40, $80, $40, $80, $40 ; P $10
	db $00, $00, $40, $3C, $84, $42, $84, $42, $A4, $52, $94, $4A, $40, $3C, $04, $02 ; Q $11
	db $00, $00, $80, $78, $88, $44, $88, $44, $80, $78, $90, $48, $88, $44, $88, $44 ; R $12
	db $00, $00, $40, $3C, $80, $40, $80, $40, $40, $3C, $04, $02, $04, $02, $40, $3C ; S $13
	db $00, $00, $80, $7C, $20, $10, $20, $10, $20, $10, $20, $10, $20, $10, $20, $10 ; T $14
	db $00, $00, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; U $15
	db $00, $00, $88, $44, $88, $44, $50, $28, $50, $28, $50, $28, $20, $10, $20, $10 ; V $16
	db $00, $00, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $48, $34 ; W $17
	db $00, $00, $82, $41, $44, $22, $28, $14, $10, $08, $28, $14, $44, $22, $82, $41 ; X $18
	db $00, $00, $44, $22, $28, $14, $10, $08, $10, $08, $10, $08, $10, $08, $10, $08 ; Y $19
	db $00, $00, $80, $7E, $08, $04, $10, $08, $20, $10, $40, $20, $80, $40, $80, $7E ; Z $1A
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; 0 $1B // Numbers
	db $00, $00, $20, $10, $40, $30, $A0, $50, $20, $10, $20, $10, $20, $10, $80, $7C ; 1 $1C
	db $00, $00, $40, $3C, $84, $42, $84, $42, $20, $1C, $40, $20, $80, $40, $80, $7E ; 2 $1D
	db $00, $00, $80, $78, $08, $04, $08, $04, $80, $78, $08, $04, $08, $04, $80, $78 ; 3 $1E
	db $00, $00, $90, $48, $90, $48, $90, $48, $90, $48, $80, $7E, $10, $08, $10, $08 ; 4 $1F
	db $00, $00, $80, $7E, $80, $40, $80, $40, $40, $3C, $04, $02, $04, $02, $80, $7C ; 5 $20
	db $00, $00, $40, $3C, $80, $40, $80, $40, $80, $7C, $84, $42, $84, $42, $40, $3C ; 6 $21
	db $00, $00, $80, $7E, $04, $02, $08, $04, $10, $08, $20, $10, $40, $20, $80, $40 ; 7 $22
	db $00, $00, $40, $3C, $84, $42, $84, $42, $40, $3C, $84, $42, $84, $42, $40, $3C ; 8 $23
	db $00, $00, $40, $3C, $84, $42, $84, $42, $40, $3C, $04, $02, $04, $02, $40, $3C ; 9 $24
	db $F8, $F8, $FC, $FC, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $7F, $7F, $3F, $3F ; Left-Low Corner $25
	db $3F, $3F, $7F, $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FC, $FC, $F8, $F8 ; Left-Top Corner $26
	db $FC, $FC, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $7F, $7F, $3F, $3F, $1F, $1F ; Right-Top Corner $27 
	db $1F, $1F, $3F, $3F, $7F, $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FC, $FC ; Right-Low Corner $28
	db $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ; Low Bar $29
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00 ; Top Bar $2A
	db $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8 ; Left Bar $2B
	db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F ; Right Bar $2C
	db $00, $00, $10, $08, $20, $10, $40, $20, $80, $7E, $40, $20, $20, $10, $10, $08 ; Backspace $2D
	db $00, $00, $14, $0A, $24, $12, $44, $22, $80, $7E, $40, $20, $20, $10, $10, $08 ; Enter $2E
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7E, $7E ; Marker $2F
	db $00, $00, $20, $18, $20, $18, $00, $00, $00, $00, $20, $18, $20, $18, $00, $00 ; ':' $30
TilesEnd:

TilesObj:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7E, $7E ; Marker $00
TilesObjEnd:

SECTION "Tile map data", ROM0

Tilemap:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $09, $0E, $10, $15, $14, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $26, $2A, $27, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $00, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $0D, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $04, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $20, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $0F, $15, $14, $10, $15, $14, $30, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $08, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $01, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $00, $2B, $13, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2F, $2F, $2F, $2F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $08, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $05, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $26, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $27, $00, $2B, $12, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2B, $00, $01, $1A, $05, $12, $14, $19, $15, $09, $0F, $10, $2D, $2C, $00, $2B, $00, $2C, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2B, $00, $11, $13, $04, $06, $07, $08, $0A, $0B, $0C, $0D, $2F, $2C, $00, $25, $29, $28, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $2B, $00, $17, $18, $03, $16, $02, $0E, $00, $00, $00, $00, $00, $2C, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $25, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:

SECTION "Var data", ROM0

ConstVarK:
	db $d7,$6a,$a4,$78,  $e8,$c7,$b7,$56,  $24,$20,$70,$db,  $c1,$bd,$ce,$ee
	db $f5,$7c,$0f,$af,  $47,$87,$c6,$2a,  $a8,$30,$46,$13,  $fd,$46,$95,$01
	db $69,$80,$98,$d8,  $8b,$44,$f7,$af,  $ff,$ff,$5b,$b1,  $89,$5c,$d7,$be
	db $6b,$90,$11,$22,  $fd,$98,$71,$93,  $a6,$79,$43,$8e,  $49,$b4,$08,$21
	db $f6,$1e,$25,$62,  $c0,$40,$b3,$40,  $26,$5e,$5a,$51,  $e9,$b6,$c7,$aa
	db $d6,$2f,$10,$5d,  $02,$44,$14,$53,  $d8,$a1,$e6,$81,  $e7,$d3,$fb,$c8
	db $21,$e1,$cd,$e6,  $c3,$37,$07,$d6,  $f4,$d5,$0d,$87,  $45,$5a,$14,$ed
	db $a9,$e3,$e9,$05,  $fc,$ef,$a3,$f8,  $67,$6f,$02,$d9,  $8d,$2a,$4c,$8a
	db $ff,$fa,$39,$42,  $87,$71,$f6,$81,  $6d,$9d,$61,$22,  $fd,$e5,$38,$0c
	db $a4,$be,$ea,$44,  $4b,$de,$cf,$a9,  $f6,$bb,$4b,$60,  $be,$bf,$bc,$70
	db $28,$9b,$7e,$c6,  $ea,$a1,$27,$fa,  $d4,$ef,$30,$85,  $04,$88,$1d,$05
	db $d9,$d4,$d0,$39,  $e6,$db,$99,$e5,  $1f,$a2,$7c,$f8,  $c4,$ac,$56,$65
	db $f4,$29,$22,$44,  $43,$2a,$ff,$97,  $ab,$94,$23,$a7,  $fc,$93,$a0,$39
	db $65,$5b,$59,$c3,  $8f,$0c,$cc,$92,  $ff,$ef,$f4,$7d,  $85,$84,$5d,$d1
	db $6f,$a8,$7e,$4f,  $fe,$2c,$e6,$e0,  $a3,$01,$43,$14,  $4e,$08,$11,$a1
	db $f7,$53,$7e,$82,  $bd,$3a,$f2,$35,  $2a,$d7,$d2,$bb,  $eb,$86,$d3,$91
ConstVarKEnd:

ConstVarH:
	db $67, $45, $23, $01  ; h0 
	db $EF, $CD, $AB, $89  ; h1
	db $98, $BA, $DC, $FE  ; h2
	db $10, $32, $54, $76  ; h3
ConstVarHEnd:

ConstVarR:
	db 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
	db 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
	db 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
	db 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
ConstVarREnd:

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db
wLastKey: db
wLetterPos: db
wLetterPosMem: db
varI : db
varNumBlock: db
varNumBlock2: db
wOutputPos: db
