;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Mathematics Module.
;Functions: SMul, Ads, Power
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

cseg	group cseg3
cseg3	segment
assume	cs:cseg

public	SMul
public	Ads
public	Power

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Mull String procedure. (Result <- Result*Counter)
;Input:		Pointer to the last byte in Tmp string.
;		Counter length.
;		Pointer to the last byte in Counter string.
;		Tmp & source strings length.
;		Pointer to the last byte in Result string.
;Output:	The mull result, in the Result string
;Use:	Add Strings procedore.
;Flags:	No Change.
;Warning:	All the strings cahnged in this procedore!!!
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

SMul	proc
	push bp
	mov bp, sp
	push ax
	push bx
	push cx			;Save Registers value.
	push dx
	push di
	push si
	push es
	pushf
	mov ax, ds
	mov es, ax                         ;Set ES to DS
	mov di, [bp+4]                    ;DI -> tmp
	mov cx, [bp+6]                     ;CX - tmp length
	mov al, 0
	std
	repe scasb                         ;Check wheter Result string is empty (0)
	jnz psj                            ;If not, check counter
	jmp psj2                           ;If so - Result <- 0
psj:	mov di, [bp+8]                     ;DI -> Counter
	mov cx, [bp+10]                    ;CX - Counter Length
	repe scasb                         ;Check wheter counter string is empty (0)
	jnz psj3                           ;If not, start mull action
psj2:	mov di, [bp+4]                    ;DI -> Result
	mov cx, [bp+6]                     ;CX - Result length
	rep stosb                          ;Result <- 0
	jmp psje                           ;Jump to mul end
psj3:    	mov di, [bp+12]                    ;DI -> Tmp
	mov si, [bp+4]                     ;SI -> Result
	mov cx, [bp+6]                     ;Length of Result and Tmp
	rep movsb                          ;Tmp <- Result
	mov bx, [bp+8]                     ;BX -> Counter
	mov dx, [bp+10]                    ;DX - Counter length
	mov cx, [bp+6]                     ;CX - Result and Tmp Length
	mov si, [bp+4]                     ;SI -> Result
	mov di, [bp+12]                    ;DI -> Tmp

psl:	cmp byte ptr [bx], 0
	jne psjn
	mov di, bx
	mov cx, dx
	rep scasb
	mov di, [bp+12]
	mov cx, [bp+6]
	jnz psjz
	jz psje
psjn:	dec byte ptr [bx]
	cmp byte ptr [bx], 0
	jne psja
	mov di, [bp+8]
	rep scasb
	mov di, [bp+12]
	mov cx, [bp+6]		            ;Counter Decing.
	jz psje
	jmp psja
psjz:	mov byte ptr [bx], 0fh
	dec bx
	dec dx
	cmp dx, 0
	jae psl
	jmp psje
psja:	mov dx, [bp+10]
	mov bx, [bp+8]
	push cx
	push si                              	;Add to Result, Tmp
	push di
	call Ads
	jmp psl

psje:	popf
	pop es
	pop si
	pop di
	pop dx
	pop cx			             ;Restore Registers value
	pop bx
	pop ax
	pop bp
	ret 10
SMul	endp

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;Add String procedure. (Destination <- Source + Destination)
;Input:		Length of source & destination string.
;		Pointer to the last byte in Destination string.
;		Pointer to the last byte in Source string.
;Output:	Add result, in destination string.
;Flags:	No Change.
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------

Ads	proc
	push bp
	mov bp, sp
	push ax
	push bx
	push cx			             ;Save registers value.
	push dx
	push di
	push si
	push es
	pushf
	mov ax, ds
	mov es, ax
	mov si, [bp+4]                       ;SI -> Source String
	mov di, [bp+6]                       ;DI -> Destination String
	mov cx, [bp+8]                       ;CX -> Length of Source and Destination
	mov al, 0
	std
	clc
	pushf
	
pal:	cmp byte ptr[di], 0                  ;Check wheater destination byte is zero 
	jne paj3                             ;If not, Check source byte
	movsb                                ;If so, copy destination to source
	popf
	jnc paj2
paj:	inc di
	inc byte ptr [di]
	mov al, [di]
	dec di
	cmp al, 0fh			     ;CF addition
	jbe paj1
	and al, 0fh
	mov [di+1], al
	stc
	jmp paj2
paj1:	clc
paj2:	pushf
	jmp pajl
paj3:	cmp byte ptr [si], 0                  ;Check wheater source byte is zero
	je paj6                                ;If so, change pointers and check CF
	lodsb                                 ;If not, add [SI] to [DI], with CF
	popf
	adc al, [di]
	cmp al, 0fh
	jbe paj4		
	and al, 0fh
	stc                                   ;CF adjusting
	jmp paj5
paj4:	clc
paj5:	pushf
	stosb
	jmp pajl
paj6:	popf
	dec di
	dec si			              ;Case Source bnyte is zero
	jnc paj7
	jmp paj
paj7:	pushf
pajl:	loop pal

	popf
	popf
	pop es
	pop si
	pop di
	pop dx			              ;Restore registers value.
	pop cx
	pop bx
	pop ax
	pop bp
	ret 6
Ads	endp

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;Power Caculating procedure (Result <- x^y).
;Input:	Result string size.
;	Offset of result string
;	Y, in the lsb (msb is zero).
;	X string size.
;	Offset of x string
;Output:	In result string.
;Use: Add String Procedure
;Flags:	N.C.
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------

Power	proc
	push bp
	mov bp, sp
	push ax
	push bx
	push cx				       ;Save registers value
	push dx
	push di
	push si
	push es
	pushf
	mov ax, ds
	mov es, ax
	mov si, [bp+4]                         ;SI -> X
	mov bx, [bp+6]                         ;BX - X length
	add si, bx
	dec si                                 ;SI -> X last byte
	mov al, [si]
	mov ah, [si-1]
	mov dx, ax
	mov al, ah
	xor ah, ah                             ;Add X two digits to AL
	mov bl, 10h
	mul bl
	add al, dl
	mov dx, ax                             ;Save X value in DL
	mov ax, [bp+8]                         ;AL = n value
	mov di, [bp+10]                        ;DI -> Result
	mov cx, [bp+12]                        ;CX - Result Lenght
	add di, cx                             ;DI -> Result last byte
	dec di
	std
	
	cmp al, 0
	jne ppjyn
	mov byte ptr [di], 1
	jmp ppje				;Special values (n): 1 and 0.
ppjyn:	cmp al, 1
	jne ppjcx
	movsb
	movsb 
	jmp ppje
	
ppjcx:	cmp dl, 1
	jne ppjxn
	mov byte ptr [di], 1
	jmp ppje
ppjxn:	cmp dl, 0			       ;Special values (x): 1 and 0
	jne ppjl
	mov byte ptr [di], 0
	jmp ppje
	
ppjl:	push si
	push di
	dec cx
	rep movsb                              ;X -> Result
	mov cx, [bp+12]
	pop si                                 ;SI <-> DI, SI -> Result, DI -> X
	pop di
	mov bl, 1                              ;BL - external counter
pple:	mov bh, 1                              ;BH - inside counter
	push si
	push di
	dec cx                                 ;Result -> X
	rep movsb
	mov cx, [bp+12]
	pop di
	pop si
	dec cx
ppli:	push cx
	push si
	push di
	call Ads                               ;Result <- X + Result
	inc bh
	cmp bh, dl                             ;If BH < X loop
	jb ppli
	inc bl
	cmp bl, al                             ;If BL < N loop
	jb pple
	
	xor al, al
	mov cx, [bp+12]
	dec cx
	push di
	rep stosb                              ;Restore X value
	pop di
	mov al, dl
	xor ah, ah
	mov cl, 10h
	div cl
	mov [di], ah
	mov [di-1], al
	
ppje:	popf
	pop es
	pop si
	pop di
	pop dx
	pop cx				;Restore registers values
	pop bx
	pop ax
	pop bp
	ret 10
Power	endp
cseg3	ends
end