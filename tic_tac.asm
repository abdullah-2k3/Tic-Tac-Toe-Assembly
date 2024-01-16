[org 0x0100]
	jmp start

;;;;;;;;;;;;;;;;;;;;;;Loading ;;;;;;;;;;;;;;;;;;;;
loading:
 push es
 push ax
 push di 
 mov  ax, 0xb800
 mov  es, ax   
 mov ax,0x4720 
 mov di,1798
 mov cx,44

 load:
 mov[es:di],ax
 mov[es:di+160],ax
 mov[es:di+160],ax
 
 call delay
 add di,2
 loop load

 pop di
 pop ax
 pop es
 ret
 ;;;;;;;;;;;;;;;;;;loading bar;;;;;;;;;;;;;;;;;;;;;;;
 bar:
 push bp
 mov  bp, sp
 push es
 push ax
 push cx
 push si
 push di

 mov ax,0xb800
 mov es,ax
 mov ah,0x06
 mov di,1498
 mov si,[bp+4]
 mov cx,10

 .nextchar:
 mov al,[si]
 mov  [es:di], ax
 add  di, 2
 add  si, 1
 loop .nextchar

 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 2
 ;;;;;;;;;;;;;;;;;;;;;;;;;;sound;;;;;;;;;;;;;;;;;;;;;;
sound:
	push bp
	mov bp, sp
	push ax

	mov al, 182
	out 0x43, al
	mov ax, [bp + 4]   ; frequency
	out 0x42, al
	mov al, ah
	out 0x42, al
	in al, 0x61
	or al, 0x03
	out 0x61, al
call delay
call delay
call delay
call delay
call delay
call delay
	in al, 0x61

	and al, 0xFC
	out 0x61, al

	pop ax
	pop bp
    ret 2
  
;;;;;;;;;;;;;;;;;;;;;;;;;;Intro TIC TAC TOC;;;;;;;;;;;;;;;;;;;;;;
intro:
	pusha	
	mov ax, 0xb800
	mov es, ax
	mov al, ' '
	mov ah, 0x22

	mov di, 980
	
	mov cx, 7	
.l1:	
	mov [es:di], ax
	add di, 160
	call delay
	loop .l1
	
	mov di, 970
	mov cx, 12
.l2:	
	mov [es:di], ax
	add di, 2
	call delay
	loop .l2
	
	mov di, 1000
	mov cx, 7
	mov ah, 0x33
.l3: 
	mov [es:di], ax
	add di, 160
	call delay
	loop .l3
	
	mov di, 1028
	mov cx, 9
	mov ah, 0x44
.l4:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l4
	
	mov cx, 6
.l5:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l5
	
	mov cx, 10
.l6:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l6

	mov cx, 7
	mov di, 1052
	mov ah, 0x55
.l7:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l7
	
	mov cx, 12
	mov di, 1042
.l8:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l8

	mov di, 1088
	mov cx, 9
	mov ah, 0x11
.l9:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l9
	
	mov cx, 7
.l10:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l10
	
	mov di, 1088
	mov cx, 7
.l11:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l11
	
	mov di, 1552
	mov cx, 9
.l012:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l012

	mov di, 1112
	mov cx, 9
	mov ah, 0x66
.l12:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l12
	
	mov cx, 6
.l13:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l13
	
	mov cx, 10
.l14:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l14
	
	
	mov di, 2450
	mov ah, 0x11
	mov cx, 7	
.l15:	
	mov [es:di], ax
	add di, 160
	call delay
	loop .l15
	
	mov di, 2440
	mov cx, 12	
.l16:	
	mov [es:di], ax
	add di, 2
	call delay
	loop .l16

	mov di, 2486
	mov cx, 9
	mov ah, 0x77
.l17:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l17
	
	mov cx, 6
.l18:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l18
	
	mov cx, 10
	
.l19:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l19
	
	mov cx, 6
	sub di, 2
.l20:
	mov [es:di], ax
	sub di, 160
	call delay
	loop .l20
	
	mov di, 2514
	mov cx, 9
	mov ah, 0xbb
.l21:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l21
	
	mov cx, 6
.l22:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l22
	
	mov cx, 10	
.l23:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l23
	
	mov di, 2978
	mov cx, 9
.l24:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l24	
	
	
	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;clear screen;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clrscr:     
    push es
    push ax
    push di

    mov  ax, 0xb800
    mov  es, ax
    mov  di, 0

    nextloc:
        mov  word [es:di], 0x0720
        add  di, 2
        cmp  di, 4000
        jne  nextloc

    pop  di 
    pop  ax
    pop  es
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;Delay;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delay:
	push cx
	mov cx, 0xffff
d:	nop
	loop d
	pop cx
	ret

clrtext:
	push bp
	mov bp,sp
	pusha
	    push ds
		pop es
		mov di,[bp+4]
		mov cx, 0xffff
		xor al,al
		repne scasb
		mov ax, 0xffff
		sub ax, cx
		dec ax
		jz exitclr
	mov cx, ax
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+6]
	mov ah,0x00
	mov al,20h
	cld
	rep stosw
	exitclr:popa
	pop bp
	ret 4



printstr:
	push bp
	mov bp, sp
	pusha
	
	push ds
	pop es
	mov di,[bp+4]
	mov cx, 0xffff
	xor al,al
	repne scasb
	mov ax, 0xffff
	sub ax, cx
	dec ax
	jz .exit
	
	mov cx, ax
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+6]
	mov si, [bp+4]
	mov ah, 0x07
	cld
	.nextchar:
		lodsb
		stosw
		loop .nextchar
	.exit:
		popa
		pop bp
		ret 4

;;;;;;;;;;;;;;;;;;;;;;;;;;Grid;;;;;;;;;;;;;;;;;;;;;;
printbox:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+4]
	
	mov ah, 0x11 ;blue color
	mov al, ' '

	mov bx, 4
.L1:
	mov cx, 8
.l1:
	mov [es:di], ax
	add di, 2
	loop .l1
	add di, 144
	dec bx



	cmp bx, 0
	jg .L1
	
	pop di
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
	
printboard:
	pusha
	mov ax, 0xb800
	mov es, ax
	
	mov di, 846
	mov bx, 4
	mov ax, 0
.L1:
	mov cx, 4	
.l1:
	push di
	call printbox
	cmp ax, 9
	jg .char
	push di
	push ax
	call printnum 
.c:
	inc ax	
	add di, 20	; next col		
	loop .l1 
	add di, 720  ; next row
	dec bx
	cmp bx, 0
	jg .L1
	jmp .done
	
.char:
	push ax
	add ax,0x37 
	mov ah, 0x04
	mov [es:di], ax
	pop ax
	jmp .c

.done:	
	popa
	ret

printnum: push bp 
	 mov bp, sp 
	 push es 
	 push ax 
	 push bx 
	 push cx 
	 push dx 
	 push di 
	 push si
	 mov si, 0
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov ax, [bp+4] ; load number in ax 	 
	 mov cx, 0 ; initialize count of digits 
nextdigit:
	 mov bx, 10
	 mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit ; if no divide it again 
	 mov di,  [bp+6]; 
nextpos:
	 pop dx ; remove a digit from the stack 
	 mov dh, 0x04 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 ;inc si
	 loop nextpos ; repeat for all digits on stack
_pexit:
	 pop si
	 pop di 
	 pop dx 
	 pop cx 
	 pop bx 
	 pop ax 
	 pop es 
	 pop bp 
	 ret 4



print_character:
	push bp
	mov bp, sp
	push ax
	push dx
	push es
	push di
	mov ax, 0xb800
	mov es, ax 
	
	mov di, [bp+4]  ; location
	mov dx, [bp+6]	;check player
	cmp dx, 2
	je .green

.yellow:
	mov ah, 0x1e
	mov al, 'O'
	jmp .print
	
.green:
	mov ah, 0x12
	mov al, 'X'
	
	
.print:
	mov [es:di], ax
 
	
	pop di
	pop es
	pop dx
	pop ax
	pop bp
	ret 4 
	


make_move:
	push bp
	mov bp, sp
	push ax
	push bx

.m:	
	call get_input	
	mov bx, [input]
	cmp word [board+bx], 0
	jne .filled
	mov word ax, [bp+4]
	mov word [board+bx], ax
	jmp .done

.filled:	
	push 526
	mov ax, not_free
	push ax
	call printstr
	call delay
	call delay
	call delay
	push word 526
	mov ax, not_free
	push ax
	call clrtext
	jmp .m
	
.done:	
	push word [board+bx]
	mov word bx, [input]
	push word [Box_coordinates+bx]
	call print_character
	
	pop bx
	pop ax
	pop bp
	ret 2
	


get_input:
	pusha

.l1:
	xor ax, ax
	mov ah, 0
    int 0x16
	cmp ah, 0x1                 ; Esc key
    je near exitgame
    cmp al, 0x30                
    jl .l1
	cmp al, 0x39
	jle .sub_num_offset
	
.check_letter:
	cmp al, 0x41	;A
	jl .l1
	cmp al, 0x46	;F
	jg .l1
		
	sub al, 0x37 ; letter offset
	jmp .save_val

.sub_num_offset:
    sub al, 0x30
	
	
.save_val:	
	mov ah, 0	
	shl ax, 1
	mov [input], ax	
	
	popa
	ret



is_victory:
	push bp
	mov bp, sp
	pusha 
	
	mov word ax, [bp+4]
	mov bx, 0
	mov cx, 4
check_X:
	cmp [board+bx], ax
	jne _next 
	cmp [board+bx+2], ax
	jne _next 
	cmp [board+bx+4], ax
	jne _next 
	cmp [board+bx+6], ax
	jne _next
	
	
	push word 526
	mov dx, [bp+6]
	push dx
	call printstr
	popa
	add sp, 4
	push 3000
	call sound
	jmp  exitgame

	
_next:
	add bx, 8
	loop check_X
	

.vertical:
	mov bx, 0
	mov cx, 4
v1:
	cmp [board+bx], ax
	jne _next1 
	cmp [board+bx+8], ax
	jne _next1 
	cmp [board+bx+16], ax
	jne _next1 
	cmp [board+bx+24], ax
	jne _next1
	
	
	push word 526
	mov dx, [bp+6]
	push dx
	call printstr
	popa
	pop bp
	add sp, 4
	push 3000
	call sound
	jmp exitgame

	
_next1:
	add bx, 2
	loop v1
	
.left_diagonal:
	mov bx, 0

	cmp [board+bx], ax
	jne .right_diagonal
	cmp [board+bx+10], ax
	jne .right_diagonal
	cmp [board+bx+20], ax
	jne .right_diagonal
	cmp [board+bx+30], ax
	jne .right_diagonal
	
	
	push word 526
	mov dx, [bp+6]
	push dx
	call printstr
	popa
	pop bp
	add sp, 4
	push 3000
	call sound
	jmp exitgame
	
	
.right_diagonal:
	mov bx, 6

	cmp [board+bx], ax
	jne .done
	cmp [board+bx+6], ax
	jne .done
	cmp [board+bx+12], ax
	jne .done
	cmp [board+bx+18], ax
	jne .done	
	
	push word 526
	mov dx, [bp+6]
	push dx
	call printstr
	popa
	pop bp
	add sp, 4
	push 3000
	call sound
	jmp exitgame
	
.done:
	popa
	pop bp
	ret 4


start:
	call clrscr
	call intro
	mov ah, 0
	int 0x16
	call clrscr
	mov ax,message
	push ax
    call bar
	call loading 
	call delay
	mov ah, 0
	int 0x16
	call clrscr
	call printboard
	
	mov cx,8
gameLoop:
	
		push 526
		mov ax, turnP1
		push ax
		call printstr
		push word 1
		call make_move
		push 2000
		call sound
		mov ax, P2_Win
		push ax
		push word 1
		call is_victory
				
		push 526
		mov ax, turnP2
		push ax
		call printstr
		push word 2
		call make_move
		push 2500
		call sound
		mov ax, P1_Win
		push ax
		push word 2
		call is_victory
	
	loop gameLoop

	push 526
	mov ax, turnP1
	push ax
	call clrtext
	push 526
	mov ax, tie
	push ax
	call printstr
	push 3500
	call sound
	push 4000
	call sound
exitgame:
	mov ah, 0
	int 0x16
	call clrscr

	mov ax,0x4c00
	int 21h
	
	

board:	dw	 0,0,0,0,
		dw   0,0,0,0,
		dw   0,0,0,0,
		dw   0,0,0,0

Box_coordinates: dw 1172, 1192, 1212, 1232
				 dw	1972, 1992, 2012, 2032
				 dw	2772, 2792, 2812, 2832
				 dw	3572, 3592, 3612, 3632
	
character: db 'O', 'X'
Player: dw 1	
turnP1: db 'Player 1 turn',0
turnP2: db 'Player 2 turn',0
P2_Win: db 'Player 1 Won the game',0
P1_Win: db 'Player 2 Won the game',0
tie: db 'Game Drawn',0
error: db 'Invalid input',0
not_free: db 'Box is already filled',0
message:  db 'Loading...'
input: dw 0