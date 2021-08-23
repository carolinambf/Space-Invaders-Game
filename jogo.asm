STACK SEGMENT PARA STACK ;define o seg. de pilha
	DB	64	DUP	('MYSTACK ')
STACK ENDS

modo_graf MACRO ;macro para colocar em modo texto
	MOV AX,0013H	;prepara para definir o modo graf. modo gráfico 320x200, 256 colors
	INT 10H	;invoca a interrupção 10h da BIOS
ENDM

modo_texto MACRO ;macro para colocar em modo texto
	MOV AX,0002H ;prepara para definir o modo texto. repor modo texto. 80x25
	INT 10H	;invoca a interrupção 10h da BIOS
ENDM

int_tempo MACRO
	MOV  AH, 2ch ;CH = HORAS, CL = MINUTOS, CH = SEGUNDOS ---> CX 
	INT  21h 
ENDM

limpa_alien MACRO ;macro para modo texto
		XOR CX,CX
		XOR DX,DX
		MOV CL,[alienx+SI]
		MOV DL,[alieny+SI]
		CALL APAGAALIEN
		MOV [alienVivo+SI],0
		DEC nalien
		INC contador
ENDM

println MACRO ;macro para salto de linha
	LEA DX,LINE
	MOV AH,09H
	INT 21H
ENDM

printMSG MACRO msg ; macro parametro msg (texto)
	mov  ah, 9
	LEA  dx, msg
	int  21H
ENDM

desenhaPx MACRO X,Y,COLOR ;desenhar pixel na x,y com a cor definida no parametro
	XOR BX,BX
	MOV CX, X
	MOV DX, Y
	MOV AL, COLOR
	MOV AH, 0CH
	INT 10H
	XOR AH,AH
ENDM

optCorPx MACRO X,Y ; saber cor do pixel em determifim_proc coordefim_proc
	PUSH BX
	XOR BX,BX
	MOV CX, X
	MOV DX, Y
	MOV AH, 0DH
	INT 10H
	MOV AH, 00H
	POP BX
ENDM

posicionamento MACRO ;macro de posição do cursor (interrupção)
	mov  ah, 2		; posição do cursor
	xor  bh, bh		
	int  10h  
ENDM

DATA SEGMENT PARA 'DATA'
	RAND	DW 0000H
	larguraJog DW 09 ; largura da nave do jogador 
	alturaJog DW 07 ; altura do nave do jogador e dimensões da nave aliens
	
	;informações sobre naves terrestres
	ntx DW 32H,96H 
	nty DW 0AFH,0AFH
	vidaUm DB 1
	vidaDois DB 1
	pos_tiro DB 2 DUP (0)
	;informações sobre naves aliens
	alienx DB 25 DUP (?)
	alieny DB 25 DUP (?)
	alienVivo DB 25 DUP (0)
	contador DB 0
	nalien DB 0
	;constantes cores
	AZUL DW 09H
	VERDE DW 02H
	VERMELHO DW 0CH
	PRETO DW 00H

	;Variaveis auxiliares para tempo
	tempo_aux dw 0    ;sempre a incrementar
	segundos db ?         
	BUF     db 6 dup (?)
	
	;linepoint (constante)
	LINE   DB	0AH, 0DH,'$'
	
	;Variaveis relacionados com a pontuação
	pont_msg DB 'SCORE P1: ','$'
	pont2_msg DB 'SCORE P2: ','$'
	pont1	 	  DW 0
	pont1STR	  DB 4 DUP ('$')
	pont2	  	  DW 0
	pont2STR	  DB 4 DUP ('$')
	
	;informações sobre estado do jogo
	pausa DB 0
	prSair DB 0
DATA ENDS

MYCODE SEGMENT PARA 'CODE' ;define o segmento cod. para o MASM	
	JOGO PROC FAR
		ASSUME CS:MYCODE,DS:DATA,SS:STACK
		PUSH DS							;guarda na pilha o SEG.DS
		XOR AX,AX						;garante zero no AX
		PUSH AX							;guarda zero na pilha
		MOV AX, DATA 					;coloca em AX a posição dos Dados
		MOV DS,AX 						;coloca essa posição no reg. DS
		POP AX
		POP AX
		
		modo_graf
		CALL geradorPosAlien					
		CALL campo					
		CALL pontucao						
	;loop que faz o jogo correr sem parar	
mainloop:
		XOR AX,AX
		MOV AL,nalien
		CMP AL, 4
		JL gerar
		MOV AX,tempo_aux
		CMP AL,0
		JE naoGerar
		MOV BL,10
		DIV BL
		CMP AH,0
		JNE naoGerar
		MOV AL,nalien
		CMP AL, 25
		JGE naoGerar
		
gerar:
		CALL novo_alien
naoGerar:		
		MOV al, prSair
		CMP al, 1
		JE GAMEOVER ;caso carregue na tecla 'q' sai do jogo
		MOV al, pausa
		CMP al, 1
		JE parado ;caso carregue 'p' os aliens não se mexem e o tempo não corre até precionar novamente
		CALL alien_move
		CALL cronometro					;inicia o relogio
parado:		
		CALL aguarda_resp
		
		CMP [vidaUm],0
		JNE jogador1_vivo ;caso o jogador 1 estiver vivo continua
		CMP [vidaDois],0
		JE GAMEOVER ; caso os 2 jogadores estiverem mortos termina o jogo
jogador1_vivo: ;		
		jmp mainloop
		
GAMEOVER:		
		modo_texto 
		
		MOV AH,01H
		INT 21H
		
		RET
	JOGO ENDP
	
	campo PROC NEAR ;apresenta o ecrã de jogo e posiciona os jogadores e coloca 4 aliens à priori 
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
	
		CALL display	
		 
		CALL separador1
		
		XOR CX,CX
		XOR DX,DX
		
		LEA DI, ntx
		LEA SI, nty
		MOV CX,[DI]
		MOV DX,[SI]
		MOV AL, 0
		CALL inicio_jogador
		MOV CX,[DI+2]
		MOV DX,[SI+2]
		MOV AL, 1
		CALL inicio_jogador
		
		MOV SI, 0
iniciar_alien:		
		
		MOV CL,[alienx+SI]		;Posição X da nave inimiga
		MOV DL,[alieny+SI]		;Posição Y da nave inimiga
		MOV [alienVivo+SI], 1
		CALL alocar_alien
		INC nalien
		INC contador
		INC SI
		CMP SI,4
		JL iniciar_alien ;caso ainda não tenham sido inseridos os 4 aliens ele corre outra vez o código de inserção
		POP DX
		POP CX
		POP BX
		POP AX
		
		RET
	campo ENDP
	;interrupção que aguarda pela inserção de um caracter
	aguarda_resp PROC NEAR 
		xor ax,ax
		xor bx,bx
		mov ah, 01h
		int 16h
		je fim_proc ;caso não seja pressiofim_proc fim_proc sai do procedimento
		xor ah,ah
		int 16h
		call def_teclas ;
		
fim_proc:		
		RET
	aguarda_resp ENDP
	; define o que faz cada tecla
	def_teclas PROC NEAR
		xor bx,bx
		
		CMP al,'p'
		je pausa_jogo; caso o jogo esteja em pausa sai do procedimento
		CMP al,'q'
		je exit ;indica o termino do jogo
		
		jmp primeiro_jg ;definição das teclas do primeiro jogador
pausa_jogo: 
		MOV BL, pausa
		CMP BL, 0
		JE ausente ;se não estiver em pausa coloca em pausa
		DEC pausa
		JMP return ;voltar ao procedimento anterior, tendo em conta que o jump não chega ao fim do código
ausente:	
		INC pausa
		JMP return
		
exit:
		INC prSair ; variavel de estado
return:		
		RET		
primeiro_jg:
		LEA DI,ntx
		LEA SI,nty
		
		mov bl, pausa
		cmp bl, 1
		jne continua ;bloqueia as ações do jogador caso esteja em pausa
		ret
continua:		
		mov bh, vidaUm ;vida do primeiro jogador
		mov bl, vidaDois;vida do segundo jogador
		CMP bx,0	;se os jogadores estiverem mortos acaba o jogo
		je exit
		
		cmp bh,0
		je repeticao_salto	;se o jogador 1 estiver morto então não existe interação
		;TECLAS DO JOGADOR 1
		CMP al,'w'
		je cima1
		CMP al,'s'
		je baixo1
		CMP al,'a'
		je esquerda1
		CMP al,'d'
		je direita1
		CMP al,'z'
		je lazeresq1
		CMP al,'x'
		je lazercentro1
		CMP al,'c'
		je lazerdir1
repeticao_salto:		
		jmp verifica_jog2
;ACCOES DAS TECLAS DO JOGADOR 1
cima1:	
		xor cx,cx
		xor dx,dx
		mov cx, [DI]
		mov dx, [SI]
		DEC dx
		DEC dx
		xor ax,ax
		
		CALL move_jogador ;verifica e desloca para a posição 
		ret
baixo1:
		xor cx,cx
		xor dx,dx
		mov cx, [DI]
		mov dx, [SI]
		INC dx
		INC dx
		;mov bx,1
		xor ax,ax
		CALL move_jogador
		ret
esquerda1:
		xor cx,cx
		xor dx,dx
		mov cx, [DI]
		mov dx, [SI]
		DEC cx
		DEC cx
		xor ax,ax
		;mov bx,0
		CALL move_jogador
		ret
direita1:
		xor cx,cx
		xor dx,dx
		mov cx, [DI]
		mov dx, [SI]
		INC cx
		INC cx
		;mov bx,1
		xor ax,ax ; ax = 0 significa que é o jogador 1
		CALL move_jogador
		ret
lazeresq1:
		MOV CX, [DI]
		MOV DX, [SI]
		MOV AX, [AZUL]
		MOV BX, 0
		CALL tiroesquerdo
		RET
lazercentro1:
		MOV CX, [DI]
		MOV DX, [SI]
		MOV AX, [AZUL]
		MOV BX, 0
		CALL tirocentral
		RET
lazerdir1:
		MOV CX, [DI]
		MOV DX, [SI]
		MOV AX, [AZUL]
		MOV BX, 0
		CALL tirodireito
		RET

verifica_jog2:		
		CMP bl,0
		jne continua2
		ret
continua2:		
		;TECLAS DO JOGADOR 2
		CMP al,'i'
		je cima2
		CMP al,'k'
		je baixo2
		CMP al,'j'
		je esquerda2
		CMP al,'l'
		je direita2
		CMP al,'b'
		je laseresquerda2
		CMP al,'n'
		je lasercentral2
		CMP al,'m'
		je laserdireito2
		JMP end_def_teclas

;ACCOES DAS TECLAS DO JOGADOR 2
cima2:
		xor cx,cx
		xor dx,dx
		mov cx, [DI+2]
		mov dx, [SI+2]
		DEC dx
		DEC dx
		mov ax,1
		
		CALL move_jogador  
		ret
baixo2:
		xor cx,cx
		xor dx,dx
		mov cx, [DI+2]
		mov dx, [SI+2]
		INC dx
		INC dx
		mov ax,1
		
		CALL move_jogador
		ret
esquerda2:
		xor cx,cx
		xor dx,dx
		mov cx, [DI+2]
		mov dx, [SI+2]
		DEC cx
		DEC cx
		mov ax,1
		
		CALL move_jogador
		ret
direita2:
		xor cx,cx
		xor dx,dx
		mov cx, [DI+2]
		mov dx, [SI+2]
		INC cx
		INC cx
		mov ax,1 ; ax = 0  jogador 1
		
		CALL move_jogador
		ret
laseresquerda2:
		MOV CX, [DI+2]
		MOV DX, [SI+2]
		MOV AX, [VERDE]
		MOV BX, 1
		CALL tiroesquerdo
		RET
lasercentral2:
		MOV CX, [DI+2]
		MOV DX, [SI+2]
		MOV AX, [VERDE]
		MOV BX, 1
		CALL tirocentral
		RET
laserdireito2:
		MOV CX, [DI+2]
		MOV DX, [SI+2]
		MOV AX, [VERDE]
		MOV BX, 1
		CALL tirodireito
		RET

end_def_teclas:		
		RET
	def_teclas ENDP

	display PROC NEAR
		PUSH AX
		PUSH BX
		
		XOR AX,AX
		MOV AH,0BH						;configuração da palete de cores
		MOV BH,00H						;inicializa a cor de background
		MOV BL,0FFH						;background (branco)
		INT 10H							;invoca a interrupção 10h da BIOS	
		
		POP BX
		POP AX
		
		RET
	display ENDP
	;movimenta o jogador
	move_jogador PROC NEAR
		PUSH AX
		PUSH CX
		PUSH DX
		
		MOV BX,CX
		CMP BX,00H
		JLE sair_procedi ;termina o procedimento
		ADD BX,[larguraJog]
		CMP BX,0F0H
		JAE sair_procedi
		MOV BX,DX
		CMP BX,096H
		JLE sair_procedi
		ADD BX,[alturaJog]
		CMP BX,0C8H
		JAE sair_procedi
		
		LEA DI,ntx
		LEA SI,nty
		
		CMP AX,1
		JE move_jogador2
		XOR BX,BX
		MOV AX,0
		MOV CX,[DI]
		MOV DX,[SI]
		CALL apagar_jogador ;limpar o jogador 1
		POP DX
		POP CX

		MOV [ntx],CX
		MOV [nty],DX
		CALL inicio_jogador
		PUSH CX
		PUSH DX
		jmp sair_procedi
move_jogador2:
		XOR BX,BX
		MOV AX,1
		MOV CX,[DI+2]
		MOV DX,[SI+2]
		CALL apagar_jogador ;limpar o jogador 2
		POP DX
		POP CX
		MOV AX,1
		MOV [ntx+2],CX
		MOV [nty+2],DX
		CALL inicio_jogador
		PUSH CX
		PUSH DX
sair_procedi:		
		POP DX
		POP CX
		POP AX
		RET
	move_jogador ENDP
	

;desenha linha branca que separa a pontuação da área de jogo
	separador1 PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		
		XOR AX,AX
		XOR BX,BX
		MOV CX,240 						;posição da coluna em 240
		MOV DX,200 						;posição da linha em 200
desenharSeparador:	
		desenhaPx CX,DX,0FH
		
		DEC DX							;decrementa a linha até chegar ao topo do ecrã
		CMP DX, 1
		JAE desenharSeparador ;caso não tenha chegado ao topo continua a pintar um pixel branco
		
		POP DX
		POP CX
		POP BX
		POP AX
		
		RET
	separador1 ENDP
	;gerar o jogador
	inicio_jogador PROC NEAR
		PUSH AX
		PUSH BX
		PUSH SI
		LEA DI,ntx
		
		AND AX,1
		CMP AX, 1
		JE J2;salta para desenho do jogador 2
		
		XOR AX,AX
		XOR BX,BX

		MOV AX,[DI]
		ADD AX,[larguraJog]
		MOV BL,0
desenharColuna:				
		CALL desenharJogador ;desenha o jogador
		
		INC CX
		DEC AX
		CMP AX,[DI]
		JAE desenharColuna ;anda no eixo do x até alcançar a largura pretendida
		JMP fimSeparador ;sair da função
J2:
		XOR AX,AX
		XOR BX,BX
		MOV AX,[DI+2]
		ADD AX,[larguraJog]
		MOV BL,1
desenharColuna2:				
		CALL desenharJogador
		
		INC CX
		DEC AX
		CMP AX,[DI+2]
		JAE desenharColuna2
fimSeparador:		
		POP SI
		POP BX
		POP AX
		
		RET
	inicio_jogador ENDP
	
	desenharJogador PROC NEAR
		PUSH AX
		PUSH BX
		PUSH DX
		
		
		XOR AX,AX
		MOV SI,[alturaJog]
		CMP BL,1
		JAE P2
		MOV AX,[AZUL]
		JMP desenharJogadorS
P2:;selecionar a cor para o segundo jogador
		MOV AX,[VERDE]
desenharJogadorS:	;desenhas as linhas consoante a altura
		XOR BX,BX
		MOV AH, 0CH 					;desenha pixel
		INT 10H
		
		INC DX
		DEC SI
		CMP SI,1
		JAE desenharJogadorS
		
		POP DX
		POP BX
		POP AX
		
		RET
	desenharJogador ENDP
	
	apagar_jogador PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		
		LEA DI,ntx
		
		AND AX,1
		CMP AX, 1
		JE saltaJogador2 ;salta para o apagar do jogador 2
		
		XOR AX,AX
		XOR BX,BX

		MOV AX,[DI]
		MOV SI,0
		MOV BL,0
saltaColuna1:				
		CALL limparJogador ;apaga o jogador
		
		INC CX
		INC SI
		CMP SI,10
		JNE saltaColuna1 ;apaga consoante a sua largura
		JMP fimSalto ;termina a função de apagar
saltaJogador2:
		XOR AX,AX
		XOR BX,BX
		MOV AX,[DI+2]
		MOV SI,0
		MOV BL,1
saltaColuna12:				
		CALL limparJogador
		
		INC CX
		INC SI
		CMP SI,10
		JNE saltaColuna1
fimSalto:	
		POP DX
		POP CX	
		POP BX
		POP AX
		
		RET
	apagar_jogador ENDP
	
	limparJogador PROC NEAR
		PUSH AX
		PUSH BX
		PUSH DX
		PUSH SI

		XOR AX,AX
		XOR BX,BX
		MOV SI,[alturaJog]

		MOV AX,[PRETO]
limparJogadorS:	
		MOV AH, 0CH 					;desenha pixel
		INT 10H
		
		INC DX
		DEC SI
		CMP SI,1
		JAE limparJogadorS
	
		POP SI
		POP DX
		POP BX
		POP AX
		
		RET
	limparJogador ENDP
	;inicio o cronometro
	cronometro PROC NEAR
precorreTempo:   ;percorre o tempo até aparecer um segundo
		
		int_tempo
		
		cmp  dh, segundos
		je   precorreTempo  
		mov  segundos, dh
		
		inc tempo_aux
		
		xor  ax, ax  
		mov ax, tempo_aux
		mov bl, 60 
		div bl 

		lea  si, BUF  
		call converterTempo ;converte  
		
		mov  dl, 111
		mov  dh, 1
		posicionamento  
		
		printMSG BUF
		
		println
		
		RET
	cronometro ENDP
	;converte numero para string
	converterNumString PROC NEAR

		mov  bx, 10 
		mov  cx, 0 
		cmp ax, 0AH
		JAE ciclo1
		MOV dl, 0
		add  dl, 48
		mov [ si ], dl
		inc si
ciclo1:       
		mov  dx, 0 
		div  bx 
		push dx 
		inc  cx 
		cmp  ax, 0  
		jne  ciclo1  
	
ciclo2:  
		pop  dx        
		add  dl, 48 
		mov  [ si ], dl
		inc  si
		loop ciclo2  
		
		RET
	converterNumString  ENDP

	converterTempo PROC NEAR
		
		PUSH ax ;  	 
		AND ax, 0FFH ; limpa o valor em ah de forma a obter somente os minutos
		CALL converterNumString
		POP ax
		MOV bl , 03Ah ; 03Ah é equivalente na tabela ascii ':'
		MOV [si], bl
		INC si
		MOV cl, 8 ; escreve no buf os segundos
		SHR ax,cl
		CALL converterNumString
		
		RET
	converterTempo ENDP
	
; mostra a  mensagem de pontuação 
	pontucao PROC NEAR
			
		mov  dl, 111	
		mov  dh, 5		
		posicionamento
		
		printMSG pont_msg
		println
		
		mov  dl, 111
		mov dh, 6
		posicionamento
		
		mov ax, pont1
		lea si, pont1STR
		CALL converterNumString
		
		printMSG pont1STR
		println
		
		mov  dl, 111	
		mov  dh, 7		
		posicionamento
		
	    printMSG pont2_msg
		println
		
		mov  dl, 111	
		mov dh, 8	
		posicionamento
		
		mov ax, pont2
		lea si, pont2STR
		CALL converterNumString
		
		printMSG pont2STR
		println
		RET
		
	pontucao ENDP

;cria o array de posições dos aliens
	geradorPosAlien PROC NEAR
		int_tempo
		MOV RAND, DX
		MOV CX, 25
		MOV SI, 0
		
posicaoAlienLoop: 
geraNovoNum:
		XOR AX,AX
		
		MOV AL, [alienVivo + si]
		CMP AL,1
		JE contVivo
		CALL aleatorio
		MOV		AX, RAND
		MOV		BX, 0C0H
		XOR		DX, DX
		DIV		BX
		
		CMP AX,BX
		JGE geraNovoNum
		
		MOV		[alienx + SI], AL
		MOV		[alieny + SI], 1
contVivo:		
		INC SI
		LOOP posicaoAlienLoop
		MOV contador, 0
		RET
	geradorPosAlien ENDP
	
	alocar_alien PROC NEAR
		PUSH SI
		XOR BX,BX
		MOV DI,0
		MOV AX,[VERMELHO]
alturaAlien:		
		PUSH DI
		MOV DI,0
larguraAlien:		
		MOV AH, 0CH 					;desenha pixel
		INT 10H
		INC CX
		CALL colisaoAlien
		INC DI
		CMP DI,[alturaJog]
		JL larguraAlien
		SUB CX,7
		POP DI
		INC DX
		INC DI
		CMP DI,[alturaJog]
		JL alturaAlien
		
		POP SI
		RET
	alocar_alien ENDP
	
	aleatorio PROC NEAR
		MOV		AX, RAND						
		MOV		DX, 8405h						
		MUL		DX								
		ADD		AL, [segundos]					
		MOV		RAND, AX						
		RET
	aleatorio ENDP
	
	novo_alien PROC NEAR
		PUSH SI
		PUSH CX
		PUSH DX
		PUSH AX
		
		XOR AX,AX
		XOR CX,CX
		XOR DX,DX
		MOV AL, contador
		CMP AL, 25
		JL ARRAY
		CALL geradorPosAlien
		
ARRAY:		
		LEA SI, [contador]
		MOV SI, [SI+0]
		AND SI, 0FFH
		INC SI
		MOV CL,[alienx+SI]		;Posição X da nave inimiga
		MOV DL,[alieny+SI]		;Posição Y da nave inimiga
		MOV [alienVivo+SI], 1
		CALL alocar_alien
		
		INC contador
		
		INC nalien
		
		POP AX
		POP DX
		POP CX
		POP DX
		RET
	novo_alien ENDP
	
	alien_move PROC NEAR
		PUSH AX
		PUSH CX
		PUSH DX
		PUSH SI
		XOR AX,AX
		XOR DX,DX
		
		MOV SI, 0
		MOV CX, 25
proximoAlien:
		PUSH CX
		XOR CX,CX
		MOV AL, [alienVivo + SI]
		CMP AL, 1
		JNE saltaAlien
		MOV CL, [alienx + SI]
		MOV DL, [alieny + SI]
		CALL APAGAALIEN
		
		MOV CL, [alienx + SI]
		MOV DL, [alieny + SI]
		MOV BL,DL
		ADD BL,08H
		CMP BX,0C8H
		JNL END_DISPLAY
		ADD DL,01h
		
		MOV [alieny + SI],DL
		CALL alocar_alien
		JMP saltaAlien
END_DISPLAY:
		MOV [alienVivo + SI],0
		DEC nalien
		INC contador
		CALL APAGAALIEN
		
saltaAlien:		
		POP CX
		INC SI
		LOOP proximoAlien
		
		POP SI
		POP DX
		POP CX
		POP AX
		RET
	alien_move ENDP
	;limpa aliens
	APAGAALIEN PROC NEAR
		PUSH SI
		XOR BX,BX
		MOV SI,0
		MOV AX,[PRETO]
apagaAltura:		
		PUSH SI
		MOV SI,0
apagaLargura:		
		MOV AH, 0CH 					;desenha pixel
		INT 10H
		
		INC CX
		INC SI
		CMP SI,[alturaJog]
		JL apagaLargura
		SUB CX,7
		POP SI
		INC DX
		INC SI
		CMP SI,[alturaJog]
		JL apagaAltura
		
		POP SI
		RET
	APAGAALIEN ENDP
	;colisão dos aliens 
	colisaoAlien PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH SI
		
		MOV AH,0Dh
		INT 10H ; AL = COLOR
		MOV AH,00H
		
		CMP AX, [AZUl]
		JNE colisaoAlien2
		limpa_alien
		MOV AX,0
		MOV CX,[ntx]
		MOV DX,[nty]
		CALL apagar_jogador
		MOV [vidaUm],0
colisaoAlien2:		
		CMP AX, [VERDE]
		JNE semColisaoAlien
		limpa_alien
		MOV AX,1
		MOV CX,[ntx+2]
		MOV DX,[nty+2]
		CALL apagar_jogador
		MOV [vidaDois],0
semColisaoAlien:		
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	colisaoAlien ENDP
	;disparo diagonal_esquerda
	tiroesquerdo PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH SI
		PUSH DI
		
		CMP BX,1
		JE jogador2
		MOV DI,0
		JMP desenhaTiro
jogador2:	
		MOV DI,2
desenhaTiro:		
		ADD CX,3
		DEC DX
		MOV BX,AX
		
		optCorPx CX,DX
		CMP AX,[VERDE]
		JNE comecaDesenhar
		CMP AX, [AZUL]
		JNE comecaDesenhar
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
comecaDesenhar:
		
		MOV AX, BX
		desenhaPx CX,DX,AL
		
contDesenharEsquerda:
		PUSH AX
		PUSH CX
		PUSH DX
tempo_espera:		
		int_tempo
		XOR AH,AH
		MOV AL,AH
		MOV CL,01H
		DIV CL
		CMP AH,0
		JNZ tempo_espera
		POP DX
		POP CX
		POP AX
		CALL diagonalEsquerda
		CMP SI,1
		JE verificarColisao
		MOV BX, AX
		CMP CX,1
		JNA paraDesenhoEsquerda
		CMP DX,1
		JA contDesenharEsquerda
		
verificarColisao:		
		CMP AX, [VERDE]
		JE paraDesenhoEsquerda
		CMP AX, [AZUL]
		JE paraDesenhoEsquerda
		CALL destruirAlien
paraDesenhoEsquerda:
		MOV [pos_tiro+DI],DL
		MOV BX,0 ;INDICADOR DA DIREÇÃO DO TIRO
		CALL limparTiroEsquerdo
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	tiroesquerdo ENDP
	;desenha o tiro 
	diagonalEsquerda PROC NEAR
		PUSH AX
		PUSH BX
		
		MOV BX,AX
		DEC DX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE pararDesenh
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV BX, AX
		DEC DX
		DEC CX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE pararDesenh
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV SI,0
		JMP sairProcEsq
pararDesenh:
		MOV SI,1	
sairProcEsq:		
		POP BX
		POP AX
		RET
	diagonalEsquerda ENDP
	;apaga o laser
	limparTiroEsquerdo PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH DI
		
		MOV CX, [ntx+DI]
		MOV DX, [nty+DI]
		
		ADD CX,3
		DEC DX
		desenhaPx CX,DX,00H
contLimparTiro:		
		DEC DX
		desenhaPx CX,DX,00H
		
		DEC DX
		DEC CX
		desenhaPx CX,DX,00H
		CMP DL,[pos_tiro+DI]
		JNE contLimparTiro
		
		POP DI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	limparTiroEsquerdo ENDP
	;destroi o alien
	destruirAlien PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		XOR AX,AX
		CMP DI,0
		JNE pont_jogador2
		INC pont1
		JMP apagarAlienn
pont_jogador2:
		INC pont2
apagarAlienn:
		MOV DI,0
procurarAlien:		
		CMP [alienVivo+DI],0
		JE proximoAlienn
		MOV AL,[alienx+DI]
		CMP AX, CX
		JG proximoAlienn
		ADD AX,07H
		CMP AX,CX
		JL proximoAlienn
		MOV AL,[alieny+DI]
		CMP AX, CX
		JG proximoAlienn
		ADD AX,07H
		CMP AX,CX
		JGE fimAlienn
		
proximoAlienn:
		INC DI
		CMP DI,25
		JL procurarAlien
fimAlienn:		
		XOR CX,CX
		XOR DX,DX
		MOV CL,[alienx+DI]
		MOV DL,[alieny+DI]
		CALL APAGAALIEN
		MOV [alienVivo+DI],0
		DEC nalien
		POP DX
		POP CX
		RET
	destruirAlien ENDP
	;diaparo central
	tirocentral PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH SI
		PUSH DI
		
		CMP BX,1
		JE jogador2_C
		MOV DI,0
		JMP desenhaTiro_C
jogador2_C:	
		MOV DI,2
desenhaTiro_C:		
		ADD CX,5
		DEC DX
		MOV BX,AX
		
		optCorPx CX,DX
		CMP AX,[VERDE]
		JNE inicioTiroCentral
		CMP AX, [AZUL]
		JNE inicioTiroCentral
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
inicioTiroCentral:
		
		MOV AX, BX
		desenhaPx CX,DX,AL
		
continuarDesenhoCentral:
		PUSH AX
		PUSH CX
		PUSH DX
ctempo_espera:		
		int_tempo
		XOR AH,AH
		MOV AL,AH
		MOV CL,01H
		DIV CL
		CMP AH,0
		JNZ ctempo_espera
		POP DX
		POP CX
		POP AX
		CALL meioCentral
		CMP SI,1
		JE pararColisaoCentral
		MOV BX, AX
		CMP CX,1
		JNA pararDesenharCentral
		CMP DX,1
		JA continuarDesenhoCentral
		
pararColisaoCentral:		
		CMP AX, [VERDE]
		JE pararDesenharCentral
		CMP AX, [AZUL]
		JE pararDesenharCentral
		CALL destruirAlien
pararDesenharCentral:
		MOV [pos_tiro+DI],DL
		MOV BX,0
		CALL limparTiroCentral
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	tirocentral ENDP
	
	meioCentral PROC NEAR
		PUSH AX
		PUSH BX
		
		MOV BX,AX
		DEC DX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE pararDesenhoTiroMeio
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV BX, AX
		DEC DX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE pararDesenhoTiroMeio
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV SI,0
		JMP sairMeioCentral
pararDesenhoTiroMeio:
		MOV SI,1	
sairMeioCentral:		
		POP BX
		POP AX
		RET
	meioCentral ENDP
	
	limparTiroCentral PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH DI
		
		MOV CX, [ntx+DI]
		MOV DX, [nty+DI]
		
		ADD CX,5
		DEC DX
		desenhaPx CX,DX,00H
continuaLimparTiroCentral:		
		DEC DX
		desenhaPx CX,DX,00H
		
		DEC DX
		desenhaPx CX,DX,00H
		CMP DL,[pos_tiro+DI]
		JNE continuaLimparTiroCentral
		
		POP DI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	limparTiroCentral ENDP
	
	tirodireito PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH SI
		PUSH DI
		
		CMP BX,1
		JE jogador2_R
		MOV DI,0
		JMP desenhaTiro_R
jogador2_R:	
		MOV DI,2
desenhaTiro_R:		
		ADD CX,7
		DEC DX
		MOV BX,AX
		
		optCorPx CX,DX
		CMP AX,[VERDE]
		JNE iniciaDireira
		CMP AX, [AZUL]
		JNE iniciaDireira
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
iniciaDireira:
		
		MOV AX, BX
		desenhaPx CX,DX,AL
		
continuaDeseneharDireita:
		PUSH AX
		PUSH CX
		PUSH DX
rtempo_espera:		
		int_tempo
		XOR AH,AH
		MOV AL,AH
		MOV CL,01H
		DIV CL
		CMP AH,0
		JNZ rtempo_espera
		POP DX
		POP CX
		POP AX
		CALL DiagonalDireita
		CMP SI,1
		JE paraColisaoDireita
		MOV BX, AX
		CMP CX,1
		JNA paraDesenhoDireita
		CMP DX,1
		JA continuaDeseneharDireita
		
paraColisaoDireita:		
		CMP AX, [VERDE]
		JE paraDesenhoDireita
		CMP AX, [AZUL]
		JE paraDesenhoDireita
		CALL destruirAlien
paraDesenhoDireita:
		MOV [pos_tiro+DI],DL
		MOV BX,0
		CALL limpaTiroDireita
		POP DI
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	tirodireito ENDP
	
	DiagonalDireita PROC NEAR
		PUSH AX
		PUSH BX
		
		MOV BX,AX
		DEC DX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE paraDesenharDireitaa
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV BX, AX
		DEC DX
		INC CX
		optCorPx CX,DX
		CMP AX,[PRETO]
		JNE paraDesenharDireitaa
		MOV AX, BX
		desenhaPx CX,DX,AL
		MOV SI,0
		JMP sairColisaoDireita
paraDesenharDireitaa:
		MOV SI,1	
sairColisaoDireita:		
		POP BX
		POP AX
		RET
	DiagonalDireita ENDP
	
	limpaTiroDireita PROC NEAR
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH DI
		
		MOV CX, [ntx+DI]
		MOV DX, [nty+DI]
		
		ADD CX,7
		DEC DX
		desenhaPx CX,DX,00H
continuaLimparTiroDireito:		
		DEC DX
		desenhaPx CX,DX,00H
		INC CX
		DEC DX
		desenhaPx CX,DX,00H
		CMP DL,[pos_tiro+DI]
		JNE continuaLimparTiroDireito
		
		POP DI
		POP DX
		POP CX
		POP BX
		POP AX
		RET
	limpaTiroDireita ENDP
MYCODE	ENDS
END