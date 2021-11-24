;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Polinom caculating program.
;Create: Omer Lervi Hevroni, 11th grade.
;All procedure input is in stack.
;This program has two includes: math.h and graphics.h
;Labels Name Key:
;First Letter: M or P - signifity wheater it Macro or Procedure.
;Second Letter: Macro's/Proc.'s First letter.
;Third letter: J, L or B - Wheater it lable for Jump, Loop, or some Buffer to jumps
;Fourth letter: somthing to siginifcy it from other same labels (E - exit), if needed.
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;Clear Specific Pixel Macro
;Clear the pixel at row 0, column p
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

CSP 	macro p
	push ax
	push bx                            ;Save registers
	push dx
	mov bh, 0                          ;Page number: 0
	mov ah, 3                          ;Read cursor place, and save it
	int 10h
	push dx
	mov dh, byte ptr p
	mov dl, 0
	mov bh, 0                          ;Set cursor place to (0, p), at page 0
	mov ah, 2
	int 10h
	mov dl, ' '                        ;Print ' ' in this place
	int 21h
	pop dx
	mov bh, 0                          ;Restore cursor place, at page 0
	mov ah, 2 
	int 10h
	pop dx
	pop bx                             ;Restore registers, and exit
	pop ax
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;X Input Macro
;Insert Byte Value (Two Digits) to SI String, with X mesg.
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

XI	macro
local	mxj, mxje
	push ax
	push bx
	push cx                            ;Save Registers
	push dx
	push bp
	xor bh, bh                         ;Page number: 0
mxj:	mov ah, 3                          ;Read cursor position
	int 10h
	mov al, 1
	mov bl, 00000101b
	mov cx, xml
	mov bp, offset xm                  ;Print X input mesg. at color Porpule (Red-Blue)
	mov ah, 13h
	int 10h
	push si
	call BInput                        ;Insert X digits to SI string
	jnc mxje
	mov ah, 3
	int 10h                            ;If there is an Input Eror, Read corsur position
	mov bl, 00000100b
	mov cx, erml
	mov bp, offset erm                 ;Print eror mesg.
	mov ah, 13h
	mov al, 1
	int 10h
	jmp mxj                            ;Restart Input
mxje:	pop bp
	pop dx
	pop cx                             ;If there is no Input Problem, Restore Registers and exit
	pop bx
	pop ax
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;N Input Macro
;Insert Byte Value (Two Digits) to SI String, with N mesg.
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

NI	macro
local	mnj, mnje
	push ax
	push bx
	push cx                            ;Save Registers
	push dx
	push bp
	xor bh, bh                         ;Page number: 0
mnj:	mov ah, 3                          ;Read cursor position
	int 10h
	mov al, 1
	mov bl, 00000101b
	mov cx, nml
	mov bp, offset nm                  ;Print N input mesg. at color Porpule (Red-Blue)
	mov ah, 13h
	int 10h
	push si
	call BInput                        ;Insert N digits to SI string
	jnc mnje
	mov ah, 3
	int 10h                            ;If there is an Input Eror, Read corsur position
	mov bl, 00000100b
	mov cx, erml
	mov bp, offset erm                 ;Print eror mesg.
	mov ah, 13h
	mov al, 1
	int 10h
	jmp mnj                            ;Restart Input
mnje:	pop bp
	pop dx
	pop cx                             ;If there is no Input Problem, Restore Registers and exit
	pop bx
	pop ax
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;X String Input Macro
;Insert string value to SI, using DI tmp string, and boath in length CX
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

XIS	macro
local	mxsjl, mxsncc
	push ax
	push bx
	push cx                           ;Save Registers
	push dx
	push si
	push bp
mxsjl:	push bp
	push cx
	mov bp, offset xm
	mov cx, xml
	PCM
	mov dx, offset ent
	mov ah, 9
	int 21h
	mov si, l
	push si
	mov si, offset x
	push si
	mov si, offset itmp
	push si
	call SInput
	jnc mxsncc
	mov ah, 3
	int 10h                            ;If there is an Input Eror, Read corsur position
	mov bl, 00000100b
	mov cx, erml
	mov bp, offset erm                 ;Print eror mesg.
	mov ah, 13h
	mov al, 1
	int 10h
	jmp mxsjl
mxsncc:	mov dx, offset ent
	mov ah, 9
	int 21h
	pop bp
	pop si
	pop dx
	pop cx                             ;If there is no Input Problem, Restore Registers and exit
	pop bx
	pop ax
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;N String Input Macro
;Insert string value to SI, using DI tmp string, and both in length CX
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

NIS	macro
local	mnsjl, mnsncc
	push ax
	push bx
	push cx                           ;Save Registers
	push dx
	push si
	push bp
mnsjl:	push bp
	push cx
	mov bp, offset nm
	mov cx, nml
	PCM
	mov dx, offset ent
	mov ah, 9
	int 21h
	mov si, l+1
	push si
	mov si, offset tmp
	push si
	mov si, offset itmp2
	push si
	call SInput
	jnc mnsncc
	mov ah, 3
	int 10h                            ;If there is an Input Eror, Read corsur position
	mov bl, 00000100b
	mov cx, erml
	mov bp, offset erm                 ;Print eror mesg.
	mov ah, 13h
	mov al, 1
	int 10h
	jmp mnsjl
mnsncc:	mov dx, offset ent
	mov ah, 9
	int 21h
	pop si
	pop bp
	pop dx
	pop cx                             ;If there is no Input Problem, Restore Registers and exit
	pop bx
	pop ax
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;X String Input Macro
;Insert string value to SI, using DI tmp string, and both in length CX
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

RIS	macro
local	mrjl, mrncc
	push ax
	push bx
	push cx                           ;Save Registers
	push dx
	push si
	push bp
mrjl:	push bp
	push cx
	mov bp, offset nm
	mov cx, nml
	PCM
	mov dx, offset ent
	mov ah, 9
	int 21h
	mov si, l+2
	push si
	mov si, offset res
	push si
	mov si, offset itmp3
	push si
	call SInput
	jnc mrncc
	mov ah, 3
	int 10h                            ;If there is an Input Eror, Read corsur position
	mov bl, 00000100b
	mov cx, erml
	mov bp, offset erm                 ;Print eror mesg.
	mov ah, 13h
	mov al, 1
	int 10h
	jmp mrjl
mrncc:	mov dx, offset ent
	mov ah, 9
	int 21h
	pop si
	pop bp
	pop dx
	pop cx                             ;If there is no Input Problem, Restore Registers and exit
	pop bx
	pop ax
endm



;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;Print Color mesg. Macro
;Print the mesg. pointed by BP, in length CX, in the current cursor position, at color Blue
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

PCM	macro
	push ax
	push bx                            ;Save Registers
	push dx
	xor bh, bh
	push cx
	mov ah, 3                          ;Read corsur place
	int 10h
	pop cx
	mov bl, 00000001b
	mov ah, 13h                        ;Print the mesg.
	mov al, 1
	int 10h
	pop dx                             ;Restore registers, with BP and CX
	pop bx
	pop ax
	pop cx
	pop bp
endm

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;Start of Data Segment
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

l = 100
s = 2
m = 5
dseg	segment
 atz	db ?
 mtmp	db l+1 dup(0)
	db '$'
 mnum	db l+1 dup(0)
	db '$'
 res	db l+2 dup(0)
	db '$'
 tmp	db l+1 dup(0)
	db '$'
 x 	db l dup(0)
	db '$'
 n	db 2 dup (0)
	db '$'
 mpm	db "This option caculating polynom, and it Deratives.", 10, 13
	db "Polynom is: x^0+x^1+...+x^n.", 10, 13
	db "You need to enter n, and the x you want the Polynom & Deratives,", 10, 13
	db "until the second derative, will be caculated.", 10, 13
 mpml	dw $ - mpm
 fi	db "Welcom User!", 10, 13
 	db "Please choose an option from the menu:", 10, 13
	db " 1)Caculate polynom.", 10, 13, " 2)Caculate power (x ^ n).", 10, 13, " 3)Mull two numbers (x * n).", 10, 13
	db " 4)Add two numbers (x + n).", 10, 13, " 5)Exit (Or Esc key).", 10, 13
 fil	dw $ - fi
 epm	db "GOOD BAY!!", 10, 13, "Create: Omer L.H.", 10, 13, "Assembler Project 2007.", 10, 13
 epml	dw $ - epm
 pwm	db "This option caculate power (x ^ n).", 10, 13
 pwml	dw $ - pwm
 mlm	db "This option mull two numbers (x * n).", 10, 13
 mlml	dw $ - mlm
 adm	db "This option add two numbers (x + n).", 10, 13
 adml	dw $ - adm
 xm	db "Please enter x(in hexa): "
 xml	dw $ - xm
 nm      	db "Please enter n(in hexa): "
 nml	dw $ - nm
 der	db "Derative "
 derl	dw $ - der
 erm	db 10, 13, "Wrong Input!", 10, 13
 erml	dw $ - erm
 scm	db "The result is: "
 scml	dw $ - scm
 der2	db ":# "
 der2l	dw $ - der2
 ent	db 10, 13, '$'
 itmp	db l+1, l+2 dup(?)
 itmp2	db l+2, l+3 dup(?)
 itmp3	db l+3, l+4 dup(?)
dseg	ends

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;End of Data Segment
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

sseg	segment stack
	dw 100h dup(?)
sseg	ends

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;Start of Code Segment
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------

cseg	group cseg1
cseg1	segment

assume	cs:cseg, ds:dseg, ss:sseg


;Mathematics Module
;Functions: SMul, Ads, Power
extrn	SMul:proc
extrn	Ads:proc
extrn	Power:proc
;Graphics Module
;Functions: SInput, BInput, clrscr, Nm2scrn, Str2scrn
extrn	SInput:proc
extrn	BInput:proc
extrn	clrscr:proc
extrn	Nm2scrn:proc
extrn	Str2scrn:proc


;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Calculate Polinum Procedure - the core of the project
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

main	proc
	push ax
	push bx
	push cx
	push dx			;Save Registers value
	push si
	push di
	push bp
	pushf
	
	mov ax, ds
	mov es, ax
	std
	xor ah, ah
	mov cx, l
	xor ah, ah
	mov si, offset x
	add si, l			;X Input (Using XI macro)
	sub si, 2
	XI
	mov si, offset n		;N Input (using NI macro)
	NI
	
	mov al, n[0]
	mov cl, 2h
	mul cl
	add al, n[1]		;Add N two digits to one number (0, 3 -> 3)
	mov n[0], al
	mov n[1], 0
	call clrscr
	xor ch, ch			;CH - Derr. counter (0 >= CH <= 2)
	clc
	pushf
	push bp
	push cx
	mov bp, offset scm
	mov cx, scml		;Print Result messg. (using PCM macro)
	PCM
	mov dx, offset ent
	mov ah, 9			;Print Enter
	int 21h
pmjel:	xor cl, cl			;CL - Powers counter (0 <= CL <= n)
	push bp
	push cx
	mov bp, offset der
	mov cx, derl		;Print derr. messg. (using PCM macro)
	PCM
	mov al, ch
	push ax
	call Nm2Scrn		;Printf derr. number (CH)
	push bp
	push cx
	mov bp, offset der2
	mov cx, der2l		;Printf "#: " (using PCM)
	PCM
	
	popf
	jnc pmjn
	pushf
	jmp pmjp
pmjn:	cmp n[0], 0			;Check wheater n == 0 - the condition of the loop
	jg pmjcc
	stc
	pushf
	jmp pmjil
pmjcc:	clc 
	pushf
	
pmjil:	mov al, cl
	mov si, l+1
	push si
	push offset tmp
	push ax
	mov si, l			;Power caculating.
	push si
	mov si, offset x
	push si
	call Power
	cmp ch, 0			;Case CH Zero (Derr. 0) the coefficient is 1
	jne pmjmp
	jmp pmjnc	
				;X coefficient caculate:
pmjmp:	push cx			;Save CX value
	mov cx, l
	mov di, offset mnum
	add di, cx			;DI -> Last byte in mnum
	dec di
	mov al, 0
	rep stosb			;mnum <- 0
	mov mnum[l], 1		;mnum <- 1
	pop cx			;Restore CX value
	mov bl, cl
	inc bl			;Caculate the limits of the factorial (BL - BH)
	mov bh, bl
	add bh, ch
	push cx			;Save CX value

pmjml:	cmp bl, bh			;The condition of the loop (For loop)
	jae pmjeml
	mov atz, bl
	mov di, offset mtmp
	mov al, 0
	mov cx, l
	cld
	rep stosb
	mov di, offset mnum-2
	push di
	mov di, 1			;Factorial loop - Result in mnum.
	push di
	lea di, atz[0]
	push di
	mov di, l+1
	push di
	mov di, offset res-2
	push di
	call SMul
	inc bl
	jmp pmjml
		
pmjeml:	mov di, offset mnum-2
	push di
	mov di, l+1
	push di
	mov di, offset res-2
	push di			;tmp = mnum * tmp
	mov di, l+1
	push di
	mov di, offset x-2
	push di
	call SMul
	pop cx			;End of X coefficient caculating - Restore CX
	
pmjnc:	mov si, l
	push si
	mov si, offset res
	add si, l+1
	push si			;Res += tmp
	mov si, offset tmp
	add si, l
	push si
	call Ads
	
	inc cl
	cmp cl, n[0]		;Loop while cl != n
	jbe pmjejil
	
	dec n[0]			;n--
pmjp:	mov si, l+2
	push si
	mov si, offset res
	push si
	call Str2Scrn		;Print Res value
	mov dx, offset ent
	mov ah, 9
	int 21h
	mov ah, 9
	int 21h
	xor al, al
	push cx
	mov di, offset res		;Res <- 0
	mov cx, l+2
	rep stosb
	mov di, offset tmp		;tmp <- 0
	mov cx, l+1
	rep stosb
	std
	pop cx			;Restore CX value
	inc ch
	cmp ch, 2			;Loop while CH <= 2
	jbe pmjejel
	
	mov ah, 1
	int 21h			;Wait for the user to press any key
	popf
	
	popf
	pop bp
	pop di
	pop si
	pop dx			;Restore Registers value
	pop cx
	pop bx
	pop ax
	ret
pmjejel:	jmp pmjel
pmjejil:	jmp pmjil			;two jumps from upstaris
main	endp

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;Graphics and Mathematics Actions Procedure - The mainly procedure
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

begin:	mov ax, dseg
	mov ds, ax
	mov es, ax			;Set DS and ES to Dseg
	mov bl, s			;BL - menu options counter
pbjel:	call clrscr
	push bx
	mov bh, 0
	mov bl, 00000101b
	mov cx, fil
	mov dl, 0			;Print first messg. - In color Red-Blue
	mov dh, 0
	mov bp, offset fi
	mov al, 1
	mov ah, 13h
	int 10h
	pop bx
pbjil:	mov bh, 0
	mov ah, 3			;Read cursor position
	int 10h
	push dx
	mov dh, bl
	mov bh, 0			;Set it at row bl, column 0
	mov dl, 0
	mov ah, 2
	int 10h
	pop dx
	mov al, 0afh
	mov bh, 0			;Print ">>" in BL color value
	mov ah, 9
	mov cx, 1
	int 10h
	std
	
	mov si, offset x		;SI -> x
	mov di, si
	add si, l-2
	mov cx, l
	add di, cx			;X <- 0
	dec di
	mov al, 0
	rep stosb
	
	mov ah, 7			;Read Charecter from stdin, with no echo
	int 21h
	cmp al, 0			;Check if it double-charecter
	jne pbjndc
	mov ah, 7			;If so, rereading with no echo
	int 21h
	cmp al, 48h			;Down arrow pressed?
	jne pbjnda
	cmp bl, s+1			;Check wheater bl is in it lowest value (no more down)
	jb pbjnmd
	CSP bl			;If not, clear BL place, and restart progress
	dec bl
pbjnmd:	jmp pbjil
pbjnda:	cmp al, 50h			;Up arrow pressed?
	je pbjnua
	jmp pbjil			;If not, restart restart progress
pbjnua:	cmp bl, s+m-2		;Check wheater BL is in it highest value (no more up)
	ja pbjnmu
	CSP bl			;If not, clear BL place, and restart progress
	inc bl
pbjnmu:	jmp pbjil

pbjndc:	cmp al, 1bh			;No double case! Is it ESC key pressed?
	jne pbjnec
	jmp pbjemp			;If so, exit!
pbjnec:	cmp al, 0dh			;Enter key pressed?
	jne pbjenp
	mov al, bl
	sub al, s
	add al, 30h			;If so, change BL to a digit that significy the menu option number
	inc al
pbjenp:	cmp al, 30h			;Numbers! AL < 30? If so, restart progress
	jb pbjrp
	cmp al, 35h			;AL > 35? If so, restart progress
	ja pbjrp
	jb pbjnc
	jmp pbjemp			;AL = 35 -> EXIT (Menu option 5 is exit)
pbjrp:	jmp pbjil
pbjnc:	call clrscr			;Clear screen, and check wich menu option the user choose
	cmp al, 31h			;AL = 31 (1)?
	jne pbjnmo1
	jmp pbjcpmo			;If so, the user choose Polinum Caculation.
pbjnmo1:	cmp al, 32h			;AL = 32 (2)?
	je pbjpmo			;If so, the user choose to caculate power	
	cmp al, 33h			;AL = 33 (3)? 
	jne pbjb
	jmp pbjmmo			;If so, the user choose to mull two numbers
pbjb:	jmp pbjamo			;If no, the user choose to add two numbers

pbjpmo:	push bp
	push cx
	mov bp, offset pwm		;Print Power messg.
	mov cx, pwml
	PCM
	mov al, 0
	mov cx, l+1
	mov di, offset tmp		;tmp <- 0
	add di, cx
	dec di
	rep stosb
	XI
	mov si, offset n		;X & N input
	NI
	mov al, n[0]
	mov cl, 10h
	mul cl
	add al, n[1]
	xor ah, ah
	mov si, l+1
	push si
	mov si, offset tmp
	push si
	push ax
	mov si, l			;Power caculating (x ^ n -> tmp).
	push si
	mov si, offset x
	push si
	call Power
	push bp
	push cx
	mov bp, offset scm
	mov cx, scml
	PCM
	cld
	mov di, l+1
	push di
	mov di, offset tmp
	push di
	call Str2Scrn
	jmp pbjpep

pbjmmo:	push bp
	push cx
	mov bp, offset mlm		;Print mul messg.
	mov cx, mlml
	PCM
	mov al, 0
	mov cx, l
	mov di, offset mtmp		;mtmp <- 0
	add di, cx
	dec di
	rep stosb
	mov di, offset res
	mov cx, l+2
	add di, cx			;res <- 0
	dec di
	rep stosb
	XIS			;X & res Input
	RIS
	pushf
	cld
	mov di, offset x
	xor al, al
	mov cx, l
	rep scasb
	mov dx, cx
	inc dx
	mov di, offset res		;Check if x < res, if so, x will be the counter 
	mov cx, l+2
	rep scasb
	dec cx
	cmp dx, cx
	jge pmjrp
	mov si, offset n-2
	mov di, offset tmp-2
	mov cx, l
	std
pmlcp:	lodsb
	xchg [di], al
	mov [si+1], al		;x <=> res 
	dec di
	loop pmlcp
pmjrp:	popf
	mov si, offset mnum-2
	push si
	mov si, l+2
	push si
	mov si, offset tmp-2
	push si			;Mull caculating (x * n -> x)
	mov si, l
	push si
	mov si, offset n-2
	push si
	call SMul
	push bp
	push cx
	mov bp, offset scm
	mov cx, scml
	PCM
	mov di, offset l
	push di
	mov di, offset x
	push di
	call Str2Scrn
	jmp pbjpep
	
pbjamo:	push bp
	push cx
	mov bp, offset adm
	mov cx, adml
	PCM
	mov al, 0
	mov cx, l+1
	mov di, offset tmp		;tmp <- 0
	add di, cx
	dec di
	rep stosb
	
	XIS
	NIS
	
	mov si, l
	push si
	mov si, offset x-2
	push si
	mov si, offset n-2
	push si
	call Ads			;Add Caculating (x+tmp -> tmp)
	push bp
	push cx
	mov bp, offset scm
	mov cx, scml
	PCM
	mov di, l+1
	push di
	mov di, offset tmp
	push di
	call Str2Scrn
pbjpep:	mov dx, offset ent
	mov ah, 9
	int 21
	mov ah, 7
	int 21h
	jmp pbjel
	
pbjcpmo:	push bp
	push cx
	mov bp, offset mpm
	mov cx, mpml		;Polinum caculating - call main procedure
	PCM
	call main
	jmp pbjel
	
pbjemp:	call clrscr			;End of program
	push bp
	push cx
	mov bp, offset epm		;Print end mesg.
	mov cx, epml
	PCM
	mov ah, 7
	int 21h
	call clrscr
	mov ah, 4ch			;Exit
	int 21h
cseg1	ends
end	begin

;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------
;End of Code Segment
;------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------