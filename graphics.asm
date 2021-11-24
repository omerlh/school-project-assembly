;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Graphics Module
;Functions: SInput, BInput, clrscr, Nm2scrn, Str2scrn
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

cseg	group cseg2
cseg2	segment

public	SInput
public	BInput
public	clrscr
public	Nm2Scrn
public	Str2scrn

assume	cs:cseg

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;String Input Procedure
;This procedure gets string of characters from stdin, into tmp string, and save it.
;Input:	l - The two strings length
;	Pointer to the result string last byte
;	Pointer to the tmp string last byte
;Output:	The string of characters, in the result string
;ASSUME:	the tmp string define is: db l+1, l+2 dup(?)
;Flags:	CF set - eror in input.
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------

SInput	proc
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	push si
	push di
	push es
	pushf
	cld
	mov ax, ds
	mov es, ax
	mov si, [bp+4]
	mov di, [bp+6]
	mov cx, [bp+8]
	mov dx, si
	mov ah, 10
	int 21h
	inc si
	lodsb
	xor ch, ch
	sub cl, al
	add di, cx
	mov cl, al
psilc:	lodsb
psijn:	cmp al, 39h                      ;AL > '9'?
	ja psijlc                         ;If not, check if it a letter
	cmp al, 30h                      ;AL < '0'?
	jb psijer                         ;If so, eror!
	sub al, 30h                      ;If not, it in range. Change it to digit, and store it
	jmp psijm
psijlc:   	or al, 20h                       ;To lowercase AL
	cmp al, 61h                      ;AL < 'a'?
	jb psijer                         ;If so, eror!
	cmp al, 66h                      ;AL > 'f'
	ja psijer                         ;If so, eror
	sub al, 57h                      ;If not, it in range. Change it to digit, and store it
	jmp psijm
psijer:	xor ax, ax	
	popf
	stc			;EROR! set CF and exit
	jmp psije
psijm:	stosb
	loop psilc
	popf
	clc
psije:	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop ax
	pop bp
	ret 6
SInput	endp

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;Byte Size Hexadecimal Checked Input procedure.
;Input:	Offset of variable string (minimum 2 bytes length)
;Output:	The number that entered, in the two first bytes.
;Flags:	Carry flag on, if there is wrong input.  
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------

BInput	proc
	push bp
	mov bp, sp
	push si
	push bx			         ;Save Registers value.
	push ax
	push dx
	push di
	pushf
	
	mov bx, [bp+4]                   ;BX -> Variable
	xor si, si                       ;SI - Index of Variable
pbjl:	mov ah, 1
	int 21h                          ;Charecter input -> AL 
	cmp al, 0
	jne pbjndcc
	mov ah, 7
	int 21h
pbjndcc:	cmp al, 0dh                      ;Enter?
	jne pbjn                         ;If not, maybe it a digit
	cmp si, 1                        ;More than one digit entered?
	jne pbjer                        ;If not, eror!
	mov al, [bx+si-1]                ;Put the digit in it place
	mov [bx+si], al
	mov byte ptr [bx+si-1], 0        ;Put zero after the digit
	jmp pbjep                        ;Exit
pbjn:	cmp al, 39h                      ;AL > '9'?
	ja pbjlc                           ;If not, check if it a letter
	cmp al, 30h                      ;AL < '0'?
	jb pbjer                          ;If so, eror!
	sub al, 30h                      ;If not, it in range. Change it to digit, and store it
	jmp pbjm
pbjlc:   	or al, 20h                       ;To lowercase AL
	cmp al, 61h                      ;AL < 'a'?
	jb pbjer                         ;If so, eror!
	cmp al, 66h                      ;AL > 'f'
	ja pbjer                         ;If so, eror
	sub al, 57h                      ;If not, it in range. Change it to digit, and store it
	jmp pbjm
pbjer:	xor ax, ax	
	popf
	stc			;EROR! set CF and exit
	jmp pbje
pbjm:	mov [bx+si], al                  ;AL -> BX[SI]
	inc si
	cmp si, 2
	jb pbjl
	
	mov ah, 7
	int 21h			;Clean stdin
pbjep:	mov ah, 2
	mov dl, 10
	int 21h                          ;Enter printing
	mov dl, 13
	int 21h
	popf
	clc                              ;Clear CF
pbje:	pop di
	pop dx
	pop ax
	pop bx                           ;Restore registers value
	pop si
	pop bp
	ret 2
BInput  endp

;--------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;Clear Screen Procedure
;---------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

clrscr	proc
	push ax
	push bx
	push cx                         ;Save Registers value
	push dx
	mov ah, 0fh                     ;Get screen mode and save it in stack
	int 10h
	push ax
	mov ah, 0
	mov al, 3                       ;Set video mode to 80x25  color text
	int 10h
	mov bh, 0
	mov cx, 0
	mov dl, 24                      ;Scroll page 0 up (Clean screen)
	mov dh, 79
	mov ah, 6
	int 10h
	pop ax
	mov ah, 0                       ;Restore screen mode
	int 10h
	pop dx
	pop cx                          ;Restore registers value
	pop bx
	pop ax
	ret
clrscr	endp

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;Print Digit, in hexadecimal, to screen procedure.
;Input:	the digit, in the lsb.
;Output:	No.
;Flags:	No Change.
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------

Nm2Scrn 	proc
	push bp
	mov bp, sp
	push ax
	push bx                           ;Save registers value
	push cx
	push dx
	pushf
	
	mov ax, [bp+4]                    ;Digit -> AL
	cmp al, 9                         ;Al > 9 ?
	ja pnjl                           ;If so, it a letter
	add al, 30h                       ;If not, it number - Change it to charecter
	jmp pnjp
pnjl:   	add al, 57h                       ;Letter case - change it to lowercase charecter
pnjp:	mov ah, 9
	mov bh, 0
	mov bl, 00000010b                 ;Print the new charecter in color green to cursor
	mov cx, 1
	int 10h                           ;Inc the corsur place
	mov ah, 3
	int 10h
	inc dl
	dec ah
	int 10h
	
	popf
	pop dx
	pop cx
	pop bx                            ;Restore Registers Value
	pop ax
	pop bp
	ret 2
Nm2Scrn	endp

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;String to Screen procedure
;Input:	length of string
;	Pointer to the last byte of the string
;Output:	to screen, the string, with comma any three digits
;Uses:	Nm2Scrn procedure
;Flags:	No change
;Warning:	This procedure DON'T print enter!
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

Str2Scrn	proc
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	pushf
psjp:	cld
	mov di, [bp+4]		;DI -> String
	xor al, al
	mov cx, [bp+6]		;CX - string length
	mov bx, di
	repe scasb
	inc cx
	dec di
	mov si, di			;Seak for the start of data
	xor ah, ah
pslp:	lodsb
	push ax
	
	mov ax, si
	sub ax, [bp+6]
	cmp ax, bx
	jnz psjces
	mov dx, 4
	jmp psjncp			;Check if need to print comma
psjces:	xor dx, dx
	inc ax
	mov di, 3
	div di
	
psjncp:	call Nm2Scrn
	cmp dx, 0
	jnz psjdpc
	mov dl, ','			;If so, print comma
	mov ah, 2
	int 21h
psjdpc:	loop pslp
	popf
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
Str2Scrn	endp
cseg2	ends
end