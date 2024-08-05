;Lorenzo Pereira Piccoli Xavier
;2024-1 - Sistemas embarcados
segment code
..start:

	;--relogio--
    mov 	ax,data
    mov 	ds,ax
    mov 	ax,stack
    mov 	ss,ax
    mov 	sp,stacktop

	; salvar modo corrente de video(vendo como est� o modo de video da maquina)
				mov  		ah,0Fh
				int  		10h
				mov  		[modo_anterior],al   

	; alterar modo de video para gr�fico 640x480 16 cores
			mov     	al,12h
			mov     	ah,0
			int     	10h

	XOR 	AX, AX
    MOV 	ES, AX
    MOV     AX, [ES:intr*4];carregou AX com offset anterior
    MOV     [offset_dos], AX        ; offset_dos guarda o end. para qual ip de int 9 estava apontando anteriormente
    MOV     AX, [ES:intr*4+2]     ; cs_dos guarda o end. anterior de CS
    MOV     [cs_dos], AX
    CLI     
    MOV     [ES:intr*4+2], CS
    MOV     WORD [ES:intr*4],relogio
    STI

;--relogio--
mov byte[numbers_entered],0
mov byte[relogio_enable],1
;--relogio--
l1:
	cmp 	byte [tique], 0
	jne 	ab
	call 	converte
ab: 
	call draw_GUI
	call read_keyboard ;if have character avaible
	abc:
	cmp byte[relogio_enable],0
	je ab
    jmp 	l1

relogio:
	push	ax
	push	ds
	mov     ax,data	
	mov     ds,ax	
    
    inc	byte [tique]
    cmp	byte[tique], 18	
    jb		Fimrel
	mov byte [tique], 0
	inc byte [segundo]
	cmp byte [segundo], 60
	jb   	Fimrel
	mov byte [segundo], 0
	inc byte [minuto]
	cmp byte [minuto], 60
	jb   	Fimrel
	mov byte [minuto], 0
	inc byte [hora]
	cmp byte [hora], 24
	jb   	Fimrel
	mov byte [hora], 0	
Fimrel:
    mov		al,20h
	out		20h,al
	pop		ds
	pop		ax
	iret
	
converte:
    push 	ax
	push    ds
	mov     ax, data
	mov     ds, ax
	xor 	ah, ah
	MOV     BL, 10
	mov 	al, byte [segundo]
    DIV     BL
    ADD     AL, 30h                                                                                          
    MOV     byte [horario+6], AL
    ADD     AH, 30h
    mov 	byte [horario+7], AH
    
	xor 	ah, ah
	mov 	al, byte [minuto]
    DIV     BL
    ADD     AL, 30h                                                                                          
    MOV     byte [horario+3], AL
    ADD     AH, 30h
    mov 	byte [horario+4], AH
	
	xor 	ah, ah
	mov 	al, byte [hora]
    DIV     BL
    ADD     AL, 30h                                                                                          
    MOV     byte [horario], AL
    ADD     AH, 30h
    mov 	byte [horario+1], AH
	mov 	ah, 09h

	pusha
		mov bx, horario
		mov     	dh,14			;line 0-29
		mov     	dl,34			;column 0-79
		call print_string
	popa

	pop     ds
	pop     ax
	ret  

segment functions

draw_GUI:
	;---Inicio Borda GUI---
			; bottom GUI
			mov		byte[cor],branco_intenso
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			mov		ax,639
			push		ax
			mov		ax,0
			push		ax
			call		line
			
			; right GUI
			mov		byte[cor],branco_intenso
			mov		ax,639
			push		ax
			mov		ax,0
			push		ax
			mov		ax,639
			push		ax
			mov		ax,479
			push		ax
			call		line
			
			; top GUI
			mov		byte[cor],branco_intenso
			mov		ax,0
			push		ax
			mov		ax,479
			push		ax
			mov		ax,639
			push		ax
			mov		ax,479
			push		ax
			call		line

			; left GUI
			mov		byte[cor],branco_intenso
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			mov		ax,479
			push		ax
			call		line
	;---Fim Borda GUI---   

	;---Init Texts GUI---
	mov bx, txt_cabecalho
	mov     	dh,2			;line 0-29
	mov     	dl,14			;column 0-79
	call print_string

	mov bx, txt_menu_teclas
	mov     	dh,19			;line 0-29
	mov     	dl,32			;column 0-79
	call print_string

	mov bx, txt_q
	mov     	dh,21			;line 0-29
	mov     	dl,35			;column 0-79
	call print_string

	mov bx, txt_s
	mov     	dh,23			;line 0-29
	mov     	dl,15			;column 0-79
	call print_string

	mov bx, txt_m
	mov     	dh,25			;line 0-29
	mov     	dl,15			;column 0-79
	call print_string

	mov bx, txt_h
	mov     	dh,27			;line 0-29
	mov     	dl,17			;column 0-79
	call print_string
	;---End Texts Menu---


;_____________________________________________________________________________
;   INPUT = bx(with pointer to sting), dh(line) and dl(column)
;	OUTPUT = print on the screen a sting inputed
print_string:
	call	cursor
	mov     al,[bx]
	cmp 	al, 0x24      ; verify if the caracter is null
	je fim_print
	call	caracter
	inc     bx			;next caracter
	inc		dl			;next columm
	mov     al,[bx]
	cmp 	al, 0x24       ; verify if the caracter is null
	jne		print_string
	fim_print:
	ret
;_____________________________________________________________________________

;__________________
; Function do the processing if any button of keyboard is pressed
read_keyboard: 
	mov 	ah,0bh	;get input status -> AL = 00h if no character available, AL = FFh if character is available.
    int 	21h			; Le buffer de teclado
    cmp 	al,0xFF 
	jne fim_read_keyboard
	mov 	ah,7 ;read without echo
    mov 	al,0
	int 	21h			; Le buffer de teclado
	call TestNumber
	call TestExit
	call TestWatch
	call TestEnter
    jmp 	fim_read_keyboard
	fim_read_keyboard:
	ret
;__________________

;__________________
; INPUT = al -> string of keyboard
; OUTPUT = 
;	byte[is_number] -> 1 if is a number
; 	byte[is_number] -> 0 if is not a number
TestNumber:
	pusha

	;test if the input is a number
	cmp al,30h
	jb not_number
	cmp al,39h
	ja not_number
	cmp byte[enable],0
	je not_number

	;inc the numbers entered by the keyboard
	mov ah, byte[numbers_entered]
	add ah, 1
	mov byte[numbers_entered], ah

	cmp byte[numbers_entered],1 ;if equal 1
	jne second_number
	first_number:
	;--print number entered--
	mov byte[is_number],1
	mov dh,byte[line_print] ;linha
	mov dl,byte[columm_print] ;coluna
	mov bl,al
	call print_byte
	;--mount first in memory--
	sub al,30h
	mov byte[first],al 
	jmp end_number
	second_number:
	cmp byte[numbers_entered],2 ;if equal 2
	jne end_number
	;--print number entered--
	mov dh,byte[line_print+1] ;linha
	mov dl,byte[columm_print+1] ;coluna
	mov bl,al
	call print_byte
	;--mount first in memory--
	sub al,30h
	mov byte[second],al 
	jmp end_number
	end_number:
	popa
	ret
	not_number:
		mov byte[is_number],0
		popa
		ret
;__________________
;doc ref int 21h -> https://redirect.cs.umbc.edu/courses/undergraduate/CMSC211/fall02/burt/tech_help/int21.html

;__________________
; INPUT = byte[numbers_entered], byte[first], byte[second]
; OUTPUT = byte[relogio_enable], byte[enable] and call modify_horario
TestEnter:
	pusha
	;cmp al,0dh
	;jne not_enter
	;mov byte[numbers_entered],0

	;reset numbers_entered if greather than 3
	cmp byte[numbers_entered], 2
	jb not_reset
	;--debug--
	;--print numbers quantity entered--
	;mov dh,7			;line 0-29
	;mov dl,33			;column 0-79
	;mov ch,byte[numbers_entered]
	;add ch,30h
    ;mov bl,ch
    ;call print_byte
	;----reset numbers_enteres----
	mov byte[numbers_entered], 0

	;----mount the number----
	mov ah, 0
	mov al, byte[first]
	mov bl,10
	mul bl
	mov ah, 0
	add al,byte[second]
	cmp al,byte[watch_limit]
	ja not_valid_number
	valid_number:
	;--debug--
	;--print heart--
	mov dh,6			;line 0-29
	mov dl,39			;column 0-79
    mov bl,03h
    call print_byte
	;---enable relogio---
	mov byte[relogio_enable],1
	mov byte[enable],0
	call modify_horario
	jmp end_mount
	not_valid_number:
	;--debug--
	;--print exclamacao--
	mov dh,6			;line 0-29
	mov dl,39			;column 0-79
    mov bl,21h
    call print_byte
	jmp end_mount
	end_mount:

	jmp end_if
	not_reset:
	;--debug--
	;--print numbers quantity entered--
	;mov dh,7			;line 0-29
	;mov dl,33			;column 0-79
	;mov ch,30h
    ;mov bl,ch
    ;call print_byte
	;mov bl,byte[last_select]
	;call print_byte
	end_if:

	popa
	ret
	not_enter:
		popa
		ret
;__________________

;__________________
; INPUT = al(with number) and byte[last_select]
; OUTPUT = byte[segundo] or byte[minuto] or byte[hora]
modify_horario:
	pusha
	;Test the last option and modify the your respective variable
	cmp byte[last_select],'s'
	je s
	cmp byte[last_select],'m'
	je m
	cmp byte[last_select],'h'
	je h

	s:
		mov byte[segundo],al
		jmp end_modify_horario
	m:
		mov byte[minuto],al
		jmp end_modify_horario
	h:
		mov byte[hora],al
		jmp end_modify_horario

	end_modify_horario:
	popa
	ret

;__________________

;__________________
; INPUT = al -> string of keyboard
; OUTPUT = exit or no if the entred is 'q'
TestExit:
	pusha
	cmp al,71h ; 'q'
	je ExitService
	cmp al,51h ; 'Q'
	je ExitService
	not_ExitService:
	popa
	ret
;__________________
;__________________
;Exit function
ExitService:
fim:
	; Kill the process
	CLI
    XOR     AX, AX
    MOV     ES, AX
    MOV     AX, [cs_dos]
    MOV     [ES:intr*4+2], AX
    MOV     AX, [offset_dos]
    MOV     [ES:intr*4], AX 

	mov ax, 3 ;Clear screen
	int 10h
	mov ah,0Ah
	int 21h
	mov ax,4c00h
	int 21h
;__________________
;__________________
; INPUT = al -> string of keyboard
; OUTPUT = 
; 	byte[line_print] and byte[line_print+1] - positions of line to print preview
;	byte[columm_print] and byte[columm_print+1] - positions of column to print preview
;	byte[watch_limit] - 59 or 23
;	byte[numbers_entered] - reset the number entred
;	byte[last_select] - 's','m' or 'h'
;	byte[enable] - flag to enable/disable the read of numbers
;	byte[relogio_enable] - flag to enable/disable the interrupt of watch
; (if 's','m' or 'h' is pressed)
TestWatch:
	pusha
	cmp al,73h ; 's'
	je SButton
	cmp al,53h ; 'S'
	je SButton
	cmp al,6dh ; 'm'
	je MButton
	cmp al,4dh ; 'M'
	je MButton
	cmp al,68h ; 'h'
	je HButton
	cmp al,48h ; 'H'
	je HButton
	end_watch: ; if keyboard is nothing of buttons 's','m' and 'h'
	popa
	ret

	SButton:
	mov byte[line_print],10
	mov byte[columm_print],38
	mov byte[line_print+1],10
	mov byte[columm_print+1],39
	mov byte[watch_limit],59
	mov byte[numbers_entered],0
	mov byte[last_select],'s'
	mov byte[enable],1
	mov byte[relogio_enable],0
	jmp end_watch

	MButton:
	mov byte[line_print],11
	mov byte[columm_print],38
	mov byte[line_print+1],11
	mov byte[columm_print+1],39
	mov byte[watch_limit],59
	mov byte[numbers_entered],0
	mov byte[last_select],'m'
	mov byte[enable],1
	mov byte[relogio_enable],0
	jmp end_watch

	HButton:
	mov byte[line_print],12
	mov byte[columm_print],38
	mov byte[line_print+1],12
	mov byte[columm_print+1],39
	mov byte[watch_limit],23
	mov byte[numbers_entered],0
	mov byte[last_select],'h'
	mov byte[enable],1
	mov byte[relogio_enable],0
	jmp end_watch

;__________________

;__________________
; INPUT = bl , dh(line) and dl(column)
; OUTPUT = print the byte in the screen
print_byte:
	pusha
	call	cursor
	mov     al,bl
	call	caracter
	popa
	ret
;__________________

;***************************************************************************

;
;   fun��o cursor
;
; dh = linha (0-29) e  dl=coluna  (0-79)
cursor:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
		mov     	ah,2
		mov     	bh,0
		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________
;
;   fun��o caracter escrito na posi��o do cursor
;
; al= caracter a ser escrito
; cor definida na variavel cor
caracter:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
    		mov     	ah,9
    		mov     	bh,0
    		mov     	cx,1
   		mov     	bl,[cor]
    		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________


;_____________________________________________________________________________
;
;   fun��o plot_xy
;
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
plot_xy:
		push		bp
		mov		bp,sp
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
	    mov     	ah,0ch
	    mov     	al,[cor]
	    mov     	bh,0
	    mov     	dx,479
		sub		dx,[bp+4]
	    mov     	cx,[bp+6]
	    int     	10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		4
;_____________________________________________________________________________
;    fun��o circle
;	 push xc; push yc; push r; call circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
; cor definida na variavel cor
circle:
	push 	bp
	mov	 	bp,sp
	pushf                        ;coloca os flags na pilha
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di
	
	mov		ax,[bp+8]    ; resgata xc
	mov		bx,[bp+6]    ; resgata yc
	mov		cx,[bp+4]    ; resgata r
	
	mov 	dx,bx	
	add		dx,cx       ;ponto extremo superior
	push    ax			
	push	dx
	call plot_xy
	
	mov		dx,bx
	sub		dx,cx       ;ponto extremo inferior
	push    ax			
	push	dx
	call plot_xy
	
	mov 	dx,ax	
	add		dx,cx       ;ponto extremo direita
	push    dx			
	push	bx
	call plot_xy
	
	mov		dx,ax
	sub		dx,cx       ;ponto extremo esquerda
	push    dx			
	push	bx
	call plot_xy
		
	mov		di,cx
	sub		di,1	 ;di=r-1
	mov		dx,0  	;dx ser� a vari�vel x. cx � a variavel y
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay:				;loop
	mov		si,di
	cmp		si,0
	jg		inf       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar
inf:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar:	
	mov		si,dx
	add		si,ax
	push    si			;coloca a abcisa x+xc na pilha
	mov		si,cx
	add		si,bx
	push    si			;coloca a ordenada y+yc na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,dx
	push    si			;coloca a abcisa xc+x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do s�timo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc+x na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do oitavo octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	add		si,cx
	push    si			;coloca a ordenada yc+y na pilha
	call plot_xy		;toma conta do terceiro octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do sexto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quinto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quarto octante
	
	cmp		cx,dx
	jb		fim_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;    fun��o full_circle
;	 push xc; push yc; push r; call full_circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
; cor definida na variavel cor					  
full_circle:
	push 	bp
	mov	 	bp,sp
	pushf                        ;coloca os flags na pilha
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]    ; resgata xc
	mov		bx,[bp+6]    ; resgata yc
	mov		cx,[bp+4]    ; resgata r
	
	mov		si,bx
	sub		si,cx
	push    ax			;coloca xc na pilha			
	push	si			;coloca yc-r na pilha
	mov		si,bx
	add		si,cx
	push	ax		;coloca xc na pilha
	push	si		;coloca yc+r na pilha
	call line
	
		
	mov		di,cx
	sub		di,1	 ;di=r-1
	mov		dx,0  	;dx ser� a vari�vel x. cx � a variavel y
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay_full:				;loop
	mov		si,di
	cmp		si,0
	jg		inf_full       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar_full
inf_full:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar_full:	
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call 	line
	
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call	line
	
	cmp		cx,dx
	jb		fim_full_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay_full		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_full_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
;   fun��o line
;
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
line:
		push		bp
		mov		bp,sp
		pushf                        ;coloca os flags na pilha
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		mov		ax,[bp+10]   ; resgata os valores das coordenadas
		mov		bx,[bp+8]    ; resgata os valores das coordenadas
		mov		cx,[bp+6]    ; resgata os valores das coordenadas
		mov		dx,[bp+4]    ; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg		ax,cx
		xchg		bx,dx
		jmp		line1
line2:		; deltax=0
		cmp		bx,dx  ;subtrai dx de bx
		jb		line3
		xchg		bx,dx        ;troca os valores de bx e dx entre eles
line3:	; dx > bx
		push		ax
		push		bx
		call 		plot_xy
		cmp		bx,dx
		jne		line31
		jmp		fim_line
line31:		inc		bx
		jmp		line3
;deltax <>0
line1:
; comparar m�dulos de deltax e deltay sabendo que cx>ax
	; cx > ax
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		ja		line32
		neg		dx
line32:		
		mov		[deltay],dx
		pop		dx

		push		ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax
line4:
		push		ax
		push		dx
		push		si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul		si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1
ar1:		sub		ax,si
		sbb		dx,0
arc1:
		idiv		word [deltax]
		add		ax,bx
		pop		si
		push		si
		push		ax
		call		plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		fim_line
		inc		si
		jmp		line4

line5:		cmp		bx,dx
		jb 		line7
		xchg		ax,cx
		xchg		bx,dx
line7:
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx



		mov		si,bx
line6:
		push		dx
		push		si
		push		ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul		si
		mov		si,[deltay]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2
ar2:		sub		ax,si
		sbb		dx,0
arc2:
		idiv		word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push		di
		push		si
		call		plot_xy
		pop		dx
		cmp		si,dx
		je		fim_line
		inc		si
		jmp		line6

fim_line:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		8
;-----------------------------------------------------------------------------

segment data

	;--Init Strings GUI--
	txt_cabecalho   	db  		'TL_2024/1, Lorenzo Pereira Piccoli Xavier, SE-Matutino', "$"
	txt_menu_teclas     db      'Menu de teclas:', "$"
	txt_q        db      'q: sair', "$"
	txt_s        db      's: para o contador dos segundos e aguarda novo valor.', "$"
	txt_m        db      'm: para o contador dos minutos e aguarda novo valor.', "$"
	txt_h       db      'h: para o contador das horas e aguarda novo valor.', "$"
	;--End Strings GUI--

	;--Init variables for processing--
	is_number	db  0
	numbers_entered	dw  0

	line_print db  15,15
	columm_print db  33,34

	watch_limit db 59
	first db 0
	second db 0
	number db 0

	last_select db 's'

	enable db 0
	relogio_enable db 0
	;--End variables for processing--

	cor		db		branco_intenso

	;	I R G B COR
	;	0 0 0 0 preto
	;	0 0 0 1 azul
	;	0 0 1 0 verde
	;	0 0 1 1 cyan
	;	0 1 0 0 vermelho
	;	0 1 0 1 magenta
	;	0 1 1 0 marrom
	;	0 1 1 1 branco
	;	1 0 0 0 cinza
	;	1 0 0 1 azul claro
	;	1 0 1 0 verde claro
	;	1 0 1 1 cyan claro
	;	1 1 0 0 rosa
	;	1 1 0 1 magenta claro
	;	1 1 1 0 amarelo
	;	1 1 1 1 branco intenso

	preto		equ		0
	azul		equ		1
	verde		equ		2
	cyan		equ		3
	vermelho	equ		4
	magenta		equ		5
	marrom		equ		6
	branco		equ		7
	cinza		equ		8
	azul_claro	equ		9
	verde_claro	equ		10
	cyan_claro	equ		11
	rosa		equ		12
	magenta_claro	equ		13
	amarelo		equ		14
	branco_intenso	equ		15

	modo_anterior	db		0
	linha   	dw  		0
	coluna  	dw  		0
	deltax		dw		0
	deltay		dw		0	
	mens    	db  		'Funcao Grafica'


	;--relogio--
	eoi     	EQU 20h
    intr	   	EQU 08h
	char		db	0
	offset_dos	dw	0
	cs_dos		dw	0
	tique		db  0
	segundo		db  0
	minuto 		db  0
	hora 		db  0
	horario		db  0,0,':',0,0,':',0,0,' ', 13,'$',0

segment stack stack
    resb 256
stacktop: