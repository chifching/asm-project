TITLE Example of ASM                (asmExample.ASM)

; This program locates the cursor and displays the
; system time. It uses two Win32 API structures.
; Last update: 6/30/2005

INCLUDE Irvine32.inc

; Redefine external symbols for convenience
; Redifinition is necessary for using stdcall in .model directive 
; using "start" is because for linking to WinDbg.  added by Huang
 
main          EQU start@0

long = 76   ;一行的長度
blong = 35 	;MEDIUM難度的球拍長度
blong1 = 50	;EASY難度的球拍長度
long2 = 102

.data

consoletitle BYTE "Brick Broker",0
text BYTE "Press S to start"
texthigh BYTE "Press A to check the higest score"
text1 BYTE "Press R to restart"
text2 BYTE "E:EASY  M:MEDIUM"
text3 BYTE "Score: "
text4 BYTE "Final Score: "
text5 Byte "Highest Score: "

xyLowerBound COORD <0,5> 	;反彈的下界
xyUpperBound COORD <76,23>	;反彈的上界
xyPos COORD <38,10> 		;目前球的位置
lastxyPos COORD <38,10> 	;球的前一個位置，印空白用
boardPos COORD <30,22> 		;球拍的位置
boardblankPos COORD <29,22> ;球拍的空白處
xyPosition COORD <0,0> 		;印出磚塊的位置
textPos COORD <30,12> 		;印出中間分數的位置
textscorepos COORD <33,13> 	;打印最高分數
textS COORD <33,12>
showscore COORD <40,12> 	;印出分數的位置
textPos1 COORD <30,13>		;印出最後分數字串的位置
showscore2 COORD <44,13> 	;印出分數的位置
textpos2 COORD <40,20>      ;印出start的位置

board BYTE blong DUP(0DCh)	;球拍
board1 BYTE blong1 DUP(0DCh)
brick BYTE 3 DUP(0DCh),' '	;磚塊
ball BYTE 6fh 				;球
blank BYTE 20h 				;移動後的前一步的印出空白

moveState DWORD 0 			;紀錄移動方向的狀態
count_score BYTE 0			;紀錄分數
high_score BYTE 0			;記錄最高分數
choose BYTE 0 				;難度
boardblank BYTE ' '

outputHandle DWORD 0 		;各種output需要的參數
outputHandle2 DWORD 0
cellsWritten DWORD ?
bytesWritten DWORD 0
count DWORD 0

attributes0 WORD long DUP(03h) ;buck顏色
attributes1 WORD long DUP(04h)
attributes2 WORD long DUP(05h)
attributes3 WORD long DUP(06h)
attributes4 WORD long DUP(9)
attributes5 WORD long DUP(0Dh)
attributes6 WORD blong DUP(0Fh)
attributes7 WORD 1 DUP(9)
attributes8 WORD blong1 DUP(0Fh)


attributes9 WORD long2 DUP(0Ch) ;break顏色
attributes10 WORD long2 DUP(0Eh)
attributes11 WORD long2 DUP(0Ah)
attributes12 WORD long2 DUP(0Bh)

pic1array1 byte  "***********        ***********          *************              *               **        **       "
pic1array2 byte  "**        **       **         *         **                        ***              **       **        "
pic1array3 byte  "**        **       **         **        **                       ** **             **      **         "
pic1array4 byte  "**        **       **         **        **                      **   **            **     **          "
pic1array5 byte  "**        **       **         *         **                     **     **           **    **           "
pic1array6 byte  "***********         **********          *************         ***********          ********           "
pic1array7 byte  "**        **       **        **         **                   **         **         **      **         "
pic1array8 byte  "**        **       **        **         **                  **           **        **       **        "
pic1array9 byte  "**        **       **         **        **                 **             **       **        **       "
pic1array10 byte "**        **       **          **       **                **               **      **         **      "
pic1array11 byte "***********        **          **       *************    **                 **     **          **     "               
.code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;初始化一些值;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main PROC
Lorigin:

	;將一些座標還原成原來的位置
	mov choose , 0
	mov xyPosition.x , 0
	mov xyPosition.y , 0
	mov count_score,0

	mov xyPos.x , 38
	mov xyPos.y , 11
	mov lastxyPos.x , 38
	mov lastxyPos.y , 11
	mov boardPos.x , 30
	mov boardblankPos.x , 29
	
	mov ebx,4 ;設定預設移動方向
	mov edx,0 
	
	INVOKE SetConsoleTitle , ADDR consoletitle

	INVOKE GetStdHandle , STD_OUTPUT_HANDLE 
		mov outputHandle, eax
		mov outputHandle2 , eax
		
	call Clrscr
	

	Delaytext1:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes9, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array1, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext2:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes10, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array2, long2,xyPosition,ADDR bytesWritten 
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext3:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes11, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array3, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext4:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes12, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array4, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext5:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes9, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array5, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext6:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes10, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array6, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext7:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes11, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array7, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext8:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes12, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array8, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext9:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes9, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array9, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext10:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes10, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array10, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
	Delaytext11:
		push eax
		mov  eax,300 ;delay 0.1 sec
		call Delay
		pop eax
		INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes11, long2,xyPosition, ADDR bytesWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, ADDR pic1array11, long2,xyPosition,ADDR bytesWritten  
		inc (COORD PTR xyPosition).Y
		CALL Crlf
 
	
	mov ax,(COORD PTR xyPosition).Y
	sub ax, 11
	mov (COORD PTR xyPosition).Y, ax
	INVOKE SetConsoleCursorPosition, outputHandle, textscorepos
	INVOKE WriteConsole,outputHandle,ADDR texthigh,33,ADDR bytesWritten,0 ;印出積分榜 text
	CALL Crlf
	INVOKE SetConsoleCursorPosition, outputHandle, textPos2
	INVOKE WriteConsole,outputHandle,ADDR text,16,ADDR bytesWritten,0 ;印出start text
	CALL Crlf
	
Lhighscore:

	call ReadChar
	.IF ax == 1E61h ; a
		call Clrscr
		INVOKE SetConsoleCursorPosition, outputHandle,textS
		INVOKE WriteConsole,outputHandle,ADDR text5,15,ADDR bytesWritten,0 ;印出積分榜 text
		movzx  eax,high_score
		call WriteDec
		
		INVOKE SetConsoleCursorPosition, outputHandle, textscorepos
		INVOKE WriteConsole,outputHandle,ADDR text,16,ADDR bytesWritten,0 ;印出選擇難度 text
	.ELSE
		jmp Lstart
	.ENDIF
	
	
	
	
	

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;按S開始遊戲;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Lstart:

	call ReadChar
	.IF ax == 1F73h ; s
		call Clrscr
		INVOKE SetConsoleCursorPosition, outputHandle, textPos
		INVOKE WriteConsole,outputHandle,ADDR text2,16,ADDR bytesWritten,0 ;印出選擇難度 text
		mov count_score,0
	.ELSE
		jmp Lstart
	.ENDIF
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;按E或M選難度;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Lchoose:

	call ReadChar
	.IF ax == 1265h ; e
		call Clrscr
	.ELSEIF ax == 326Dh ; m
		call Clrscr
		mov choose , 1
	.ELSE
		jmp Lchoose ;偵測直到案了e或是m
	.ENDIF
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;印每一行的磚塊;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LPrintBrick:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第一行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INVOKE WriteConsoleOutputAttribute, outputHandle, ADDR attributes0, long,xyPosition, ADDR cellsWritten
	mov ecx , 19
	LA0:
		push ecx
		INVOKE WriteConsoleOutputCharacter, outputHandle,ADDR brick,  4,   xyPosition,  ADDR count   
		add xyPosition.x , 4
		pop ecx
		Loop LA0
		inc xyPosition.y     ;換行
		mov xyPosition.x , 2 ;下一行的起始x
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第二行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   INVOKE WriteConsoleOutputAttribute,outputHandle, ADDR attributes1,long,xyPosition, ADDR cellsWritten
	mov ecx , 19
	LA1:
		push ecx
		INVOKE WriteConsoleOutputCharacter,outputHandle, ADDR brick, 4,xyPosition,ADDR count   
		add xyPosition.x , 4
		pop ecx
		Loop LA1
		inc xyPosition.y     ;換行
		mov xyPosition.x , 0 ;下一行的起始x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第三行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   INVOKE WriteConsoleOutputAttribute,outputHandle, ADDR attributes2, long,xyPosition, ADDR cellsWritten
	mov ecx , 19
	LA2:
		push ecx
		INVOKE WriteConsoleOutputCharacter,outputHandle,ADDR brick, 4, xyPosition, ADDR count  
		add xyPosition.x , 4
		pop ecx
		Loop LA2
		inc xyPosition.y     ;換行
		mov xyPosition.x , 2 ;下一行的起始x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第四行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
   INVOKE WriteConsoleOutputAttribute, outputHandle,ADDR attributes3,long, xyPosition, ADDR cellsWritten
	mov ecx , 19
	LA3:
		push ecx
		INVOKE WriteConsoleOutputCharacter,outputHandle,ADDR brick, 4,xyPosition, ADDR count    
		add xyPosition.x , 4
		pop ecx
		Loop LA3
		inc xyPosition.y     ;換行
		mov xyPosition.x , 0 ;下一行的起始x
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第五行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
   INVOKE WriteConsoleOutputAttribute,outputHandle, ADDR attributes4,long, xyPosition,ADDR cellsWritten
	mov ecx , 19
	LA4:
		push ecx
		INVOKE WriteConsoleOutputCharacter,outputHandle,ADDR brick, 4,xyPosition, ADDR count   
		add xyPosition.x , 4
		pop ecx
		Loop LA4
		inc xyPosition.y     ;換行
		mov xyPosition.x , 2 ;下一行的起始x
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;第六行;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
   INVOKE WriteConsoleOutputAttribute,outputHandle,ADDR attributes5, long,xyPosition, ADDR cellsWritten
	mov ecx , 19
	LA5:
		push ecx
		INVOKE WriteConsoleOutputCharacter,outputHandle,  ADDR brick, 4, xyPosition,ADDR count   
		add xyPosition.x , 4
		pop ecx
		Loop LA5
		inc xyPosition.y     ;換行
		mov xyPosition.x , 0 ;下一行的起始x

		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAMELOOP移動每一步;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Movement: 
	INVOKE SetConsoleCursorPosition, outputHandle, textPos
	INVOKE WriteConsole,outputHandle,ADDR text3,7,ADDR bytesWritten,0 	;印出"SCORE:"在畫面中央
	INVOKE SetConsoleCursorPosition, outputHandle, showscore 			;接著印分數
		movzx  eax,count_score
		call WriteDec

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;印出難度MEDIUM的球拍，設定控制左右移動;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.IF choose == 1 ;選擇MEDIUM難度
		INVOKE WriteConsoleOutputAttribute,outputHandle2,ADDR attributes6,blong,boardPos,ADDR cellsWritten
		INVOKE WriteConsoleOutputCharacter,outputHandle2,  ADDR board, blong, boardPos,ADDR count    
		;球拍座標所設定的為最左邊位置 在由其下去推該印空白或球拍的情況
		push ebx
		call ReadKey ;偵測按鍵
		pop ebx
	.IF ax == 4B00h ;LEFT

		dec boardPos.x
		mov ax , boardPos.x
		add ax , blong
		mov boardblankPos.x , ax
		;設定要印出空白的位置並輸出
		INVOKE WriteConsoleOutputAttribute,outputHandle2,ADDR attributes6, 1,boardblankPos,ADDR cellsWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle2, ADDR boardblank,1, boardblankPos, ADDR count   
		 
		mov ax , 0h
		sub ax , 1
		.IF boardPos.x == ax ;如果板子碰到左邊界保持不動
			add boardPos.x,1
		.ENDIF

	.ENDIF
	
	.IF ax == 4D00h ;RIGHT

		mov ax , boardPos.x
		inc boardPos.x
		mov boardblankPos.x , ax
		;設定要印出空白的位置並輸出
		INVOKE WriteConsoleOutputAttribute,outputHandle2,ADDR attributes3,1,boardblankPos,ADDR cellsWritten
		INVOKE WriteConsoleOutputCharacter,outputHandle2, ADDR boardblank,  1,   boardblankPos, ADDR count    
		 
		mov ax , xyUpperBound.x ;如果板子碰到右邊界保值不動
		sub ax , (blong-2)
		.IF boardPos.x == ax
			sub boardPos.x,1
		.ENDIF

	.ENDIF
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;印出難度EASY的球拍，設定控制左右移動;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.ELSE ;選擇EASY難度
		INVOKE WriteConsoleOutputAttribute,outputHandle2, ADDR attributes8,blong1,boardPos,ADDR cellsWritten
		INVOKE WriteConsoleOutputCharacter,outputHandle2, ADDR board1, blong1,boardPos, ADDR count 
		;球拍座標所設定的為最左邊位置 在由其下去推該印空白或球拍的情況
		push ebx
		call ReadKey ;偵測按鍵
		pop ebx
		.IF ax == 4B00h ;LEFT
		
			dec boardPos.x
			mov ax , boardPos.x
			add ax , blong1
			mov boardblankPos.x , ax
			;設定要印出空白的位置並輸出
			INVOKE WriteConsoleOutputAttribute,outputHandle2, ADDR attributes8,1,boardblankPos, ADDR cellsWritten
			INVOKE WriteConsoleOutputCharacter,outputHandle2,  ADDR boardblank, 1,  boardblankPos, ADDR count    
			 
			mov ax , 0h
			sub ax , 1
			.IF boardPos.x == ax ;如果板子碰到左邊界保持不動
				add boardPos.x,1
			.ENDIF

		.ENDIF
		
		.IF ax == 4D00h ;RIGHT
		
			mov ax , boardPos.x
			inc boardPos.x
			mov boardblankPos.x , ax
			;設定要印出空白的位置並輸出
			INVOKE WriteConsoleOutputAttribute,outputHandle2,ADDR attributes8, 1,boardblankPos,ADDR cellsWritten
			INVOKE WriteConsoleOutputCharacter,outputHandle2, ADDR boardblank,  1,  boardblankPos,   ADDR count    
			 
			mov ax , xyUpperBound.x
			sub ax , (blong1-2)
			.IF boardPos.x == ax ;如果板子碰到右邊界保持不動
				sub boardPos.x,1
			.ENDIF
			
		.ENDIF
		
	.ENDIF
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;印出球的位置，並將前一個移動過的位置印空白;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	INVOKE SetConsoleCursorPosition, outputHandle, lastxyPos ;前一個游標位置
	INVOKE WriteConsoleOutputAttribute, outputHandle2,ADDR attributes7,1, lastxyPos,ADDR cellsWritten
	INVOKE WriteConsoleOutputCharacter,outputHandle2, ADDR blank, 1,lastxyPos,ADDR count   ;將前一個印出blank
	
	INVOKE SetConsoleCursorPosition, outputHandle, xyPos ;目前游標位置
	INVOKE WriteConsoleOutputAttribute,outputHandle2, ADDR attributes7,1,xyPos,ADDR cellsWritten
	INVOKE WriteConsoleOutputCharacter, outputHandle2, ADDR ball, 1,xyPos,ADDR count   ;將目前的印出ball

	push ax
	push bx
	mov ax,xyPos.x
	mov bx,xyPos.y
	;儲存前一個位置
	mov lastxyPos.x,ax 
	mov lastxyPos.y,bx
	pop bx
	pop ax
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;判斷球碰到邊界的反彈方向;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;比較ebx來判斷該換哪個方向
	
	mov ax,xyLowerBound.x           ;右邊界
	.IF xyPos.x == ax 
		.IF ebx==3 
			mov ebx,4 
		.ENDIF
		.IF ebx==1 
			mov ebx,2 
		.ENDIF
	.ENDIF
	
	mov ax,xyLowerBound.x
	dec ax
	.IF xyPos.x == ax 
		.IF ebx==3 
			mov ebx,4 
		.ENDIF
		.IF ebx==1 
			mov ebx,2 
		.ENDIF
	.ENDIF
	
	mov ax,xyUpperBound.x            ;左邊界
	.IF xyPos.x == ax 
		.IF ebx==4 
			mov ebx,3 
		.ENDIF
		.IF ebx==2 
			mov ebx,1 
		.ENDIF
	.ENDIF
	
	mov ax,xyUpperBound.x 
	inc ax
	.IF xyPos.x == ax 
		.IF ebx==4 
			mov ebx,3 
		.ENDIF
		.IF ebx==2 
			mov ebx,1 
		.ENDIF
	.ENDIF
	
	
	mov ax,xyLowerBound.y           ;上邊界
	.IF xyPos.y == ax 
		inc count_score
		.IF count_score==8
			dec xyLowerBound.y
		.ENDIF
		.IF count_score==9
			jmp left_down
		.ENDIF
		 .IF count_score==10
			inc xyLowerBound.y
		 .ENDIF
		
		.IF ebx==2 
			mov ebx,4 
		.ENDIF
		.IF ebx==1 
			mov ebx,3 
		.ENDIF
		
	.ENDIF
	
	
	mov ax,xyUpperBound.y            ;下邊界
	.IF xyPos.y == ax 
		INVOKE SetConsoleCursorPosition, outputHandle, textPos
		INVOKE WriteConsole,outputHandle,ADDR text1,18,ADDR bytesWritten,0 ;印出restart的text
		INVOKE SetConsoleCursorPosition, outputHandle, textPos1
		INVOKE WriteConsole,outputHandle,ADDR text4,13,ADDR bytesWritten,0 ;印出最後分數字串
		INVOKE SetConsoleCursorPosition, outputHandle, showscore2 		   ;接著印分數
		movzx  eax,count_score
		.IF al >= high_score
			mov  high_score,al
		.ENDIF
		call WriteDec
		
		Lrestart:              ;球沒接到，跳Restart畫面
			call ReadChar
			.IF ax == 1372h ;r
				jmp Lorigin
			.ELSE
				jmp Lrestart ;偵測直到按了r
			.ENDIF
		
		.IF ebx==3 
			mov ebx,1 
		.ENDIF
		.IF ebx==4 
			mov ebx,2 
		.ENDIF
	.ENDIF
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;偵測有沒有接到球，再judge反彈方向;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	push bx
	mov bx,boardPos.y
	.IF xyPos.y == bx  ;球的y座標與板子的y座標相比
		mov ax,boardPos.x
		.IF xyPos.x >= ax ;看球的x座標有沒有在板子的範圍內
			.IF choose ==1	;MEDIUM的情況
				add ax , (blong-1)
			.ELSE			;EASY的情況
				add ax , (blong1-1)
			.ENDIF
			.IF xyPos.x <= ax ;看球的x座標有沒有在板子的範圍內
				pop bx
			.IF bx == 3
				sub xyPos.y , 2 ;先將其y座標往上移保持與板子的距離
				jmp judge ;偵測該如何移動
			.ENDIF
			.IF bx == 4
				sub xyPos.y , 2 ;先將其y座標往上移保持與板子的距離
				jmp judge ;偵測該如何移動
			.ENDIF
				
			.ENDIF
		.ENDIF
	.ENDIF
	pop bx
	
	jmp skip_judge ;若沒有偵測到變化則不改變移動模式
	
	judge: ;根據相對應移動方向變化該如何移動
		.IF ebx==3 ;左下
			mov ebx,1 ;左上
		.ELSEIF ebx==2 ;右上
			mov ebx,4 ; 右下
		.ELSEIF ebx==1 ;左上
			mov ebx,3 ;左下
		.ELSEIF ebx==4 ;右下
			mov ebx,2 ;右上
		.ENDIF

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;將移動方向紀錄至movestate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	skip_judge:
	
		mov moveState,ebx
		
		.IF moveState==1
			jmp left_up
		.ENDIF

		.IF moveState==2
			jmp right_up
		.ENDIF

		.IF moveState==3
			jmp left_down
		.ENDIF

		.IF moveState==4
			jmp right_down
		.ENDIF


	left_up: ;左上的移動方式
		dec xyPos.x
		dec xyPos.y
		jmp Delay0
	right_up: ;右上的移動方式
		inc xyPos.x
		dec xyPos.y
		jmp Delay0
	left_down: ;左下的移動方式
		dec xyPos.x
		inc xyPos.y
		jmp Delay0
	right_down: ;右下的移動方式
		inc xyPos.x
		inc xyPos.y
		jmp Delay0
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAMELOOP時間延遲;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
		

		
	Delay0:
		push eax
		mov  eax,60 ;delay 0.1 sec
		call Delay
		pop eax

		push eax
		INVOKE GetKeyState,CAPSLOCK_ON   ;利用KeyEvent的特性，未觸發就不會改變旗標，使球不斷移動
		test al,1
		.IF !Zero?
			pop eax
			jmp Movement ;繼續迴圈
		.ENDIF
		pop eax
		jmp Movement ;繼續迴圈
		
		call Clrscr

		exit
	
main ENDP



END main























