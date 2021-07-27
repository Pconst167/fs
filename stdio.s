;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stdio.s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.include "string.s"

s_hex_digits:	.db "0123456789ABCDEF"	

table_power:	.dw 1
		.dw 10
		.dw 100
		.dw 1000
		.dw 10000
		.dw 10000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONVERT ASCII 'O'..'F' TO INTEGER 0..15
; ASCII in BL
; result in AL
; ascii for F = 0100 0110
; ascii for 9 = 0011 1001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
hex_ascii_encode:
	mov al, bl
	test al, 40h				; test if letter or number
	jnz hex_letter
	and al, 0Fh				; get number
	ret
hex_letter:
	and al, 0Fh				; get letter
	add al, 9
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ATOI
; 2 letter hex string in B
; 8bit integer returned in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
atoi:
	mov g, b
	call hex_ascii_encode			; convert BL to 4bit code in AL
	mov bl, bh
	push al					; save a
	call hex_ascii_encode
	pop bl	
	shl al, 4
	or al, bl
	mov b, g
	ret	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; printf
; no need for explanations!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printf:


	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scanf
; no need for explanations!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scanf:


	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ITOA
; 8bit value in BL
; 2 byte ASCII result in A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
itoa:
	push d
	mov g, b
	mov bh, 0
	shr bl, 4	
	mov d, b
	mov al, [d + s_hex_digits]
	mov ah, al
	
	mov b, g
	mov bh, 0
	and bl, $0F
	mov d, b
	mov al, [d + s_hex_digits]
	mov b, g
	pop d
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HEX STRING TO BINARY
; di = destination address
; si = source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
hex_to_int:
hex_to_int_L1:
	lodsb					; load from [SI] to AL
	cmp al, 0				; check if ASCII 0
	jz hex_to_int_ret
	mov bh, al
	lodsb
	mov bl, al
	call atoi				; convert ASCII byte in B to int (to AL)
	stosb					; store AL to [DI]
	jmp hex_to_int_L1
hex_to_int_ret:
	ret		



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GETCHAR
; char in ah
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getchar:
	push al
	mov al, 1
	syscall sys_io			; receive in AH
	pop al
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUTCHAR
; char in ah
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putchar:
	push a
	mov al, 0
	syscall sys_io			; char in AH
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT A STRING
;; terminates with null
;; pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gets:
	push a
	push d
gets_loop:
	sti
	mov al, 1
	syscall sys_io			; receive in AH

	cmp ah, 0Ah				; LF
	je gets_end
	cmp ah, $5C				; '\\'
	je gets_escape
	
	cmp ah, $08			; check for backspace
	je gets_backspace

	mov al, ah
	mov [d], al
	inc d
	jmp gets_loop
gets_backspace:
	dec d
	jmp gets_loop
gets_escape:
	mov al, 1
	syscall sys_io			; receive in AH
	cmp ah, 'n'
	je gets_LF
	cmp ah, 'r'
	je gets_CR
	mov al, ah				; if not a known escape, it is just a normal letter
	mov [d], al
	inc d
	jmp gets_loop
gets_LF:
	mov al, $0A
	mov [d], al
	inc d
	jmp gets_loop
gets_CR:
	mov al, $0D
	mov [d], al
	inc d
	jmp gets_loop
gets_end:
	mov al, 0
	mov [d], al				; terminate string
	pop d
	pop a
	ret




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT TEXT
;; terminated with CTRL+D
;; pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gettxt:
	push a
	push d
gettxt_loop:
	sti
	mov al, 1
	syscall sys_io			; receive in AH
	cmp ah, 4			; EOT
	je gettxt_end
	cmp ah, $08			; check for backspace
	je gettxt_backspace
	cmp ah, $5C				; '\\'
	je gettxt_escape
	mov al, ah
	mov [d], al
	inc d
	jmp gettxt_loop
gettxt_escape:
	mov al, 1
	syscall sys_io			; receive in AH
	cmp ah, 'n'
	je gettxt_LF
	cmp ah, 'r'
	je gettxt_CR
	mov al, ah				; if not a known escape, it is just a normal letter
	mov [d], al
	inc d
	jmp gettxt_loop
gettxt_LF:
	mov al, $0A
	mov [d], al
	inc d
	jmp gettxt_loop
gettxt_CR:
	mov al, $0D
	mov [d], al
	inc d
	jmp gettxt_loop
gettxt_backspace:
	dec d
	jmp gettxt_loop
gettxt_end:
	mov al, 0
	mov [d], al				; terminate string
	pop d
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRINT NEW LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printnl:
	push a
	mov a, $0A00
	syscall sys_io
	mov a, $0D00
	syscall sys_io
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strtoint
; 4 digit string number in d
; integer returned in A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strtoint:
;	push b
	mov bl, [d]
	mov bh, bl
	mov bl, [d + 1]
	call atoi				; convert to int in AL
	mov ah, al				; move to AH
	mov bl, [d + 2]
	mov bh, bl
	mov bl, [d + 3]
	call atoi				; convert to int in AL
;	pop b
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRINT NULL TERMINATED STRING
; pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
puts:
	push a
	push d
puts_L1:
	mov al, [d]
	cmp al, 0
	jz puts_END
	mov ah, al
	mov al, 0
	syscall sys_io
	inc d
	jmp puts_L1
puts_END:
	pop d
	pop a
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; print 16bit decimal number
; input number in A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_u16d:
	push a
	push b

	mov b, 10000
	div a, b			; get 10000 coeff.
	call print_zero_or_space
	mov a, b

	mov b, 1000
	div a, b			; get 10000 coeff.
	call print_zero_or_space
	mov a, b

	mov b, 100
	div a, b
	call print_zero_or_space
	mov a, b

	mov b, 10
	div a, b
	call print_zero_or_space
	mov a, b

	mov al, bl
	add al, $30
	mov ah, al
	mov al, 0
	syscall sys_io	; print coeff
	pop b
	pop a
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if A == 0, print space
; else print A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_zero_or_space:
	; cmp a, 0
	; jne print_number
	; mov ah, $20
	; call putchar
	; ret
print_number:
	add al, $30
	mov ah, al
	call putchar
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRINT 16BIT HEX INTEGER
; integer value in reg B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_u16x:
	push a
	push b
	push bl
	mov bl, bh
	call itoa				; convert bh to char in A
	mov bl, al				; save al
	mov al, 0
	syscall sys_io				; display AH
	mov ah, bl				; retrieve al
	mov al, 0
	syscall sys_io				; display AL

	pop bl
	call itoa				; convert bh to char in A
	mov bl, al				; save al
	mov al, 0
	syscall sys_io				; display AH
	mov ah, bl				; retrieve al
	mov al, 0
	syscall sys_io				; display AL

	pop b
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INPUT 16BIT HEX INTEGER
; read 16bit integer into A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scan_u16x:
	enter 16
	push b
	push d

	lea d, [bp + -15]
	call gets				; get number

	mov bl, [d]
	mov bh, bl
	mov bl, [d + 1]
	call atoi				; convert to int in AL
	mov ah, al				; move to AH

	mov bl, [d + 2]
	mov bh, bl
	mov bl, [d + 3]
	call atoi				; convert to int in AL

	pop d
	pop b
	leave
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRINT 8bit HEX INTEGER
; integer value in reg bl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_u8x:
	push a
	push bl

	call itoa				; convert bl to char in A
	mov bl, al				; save al
	mov al, 0
	syscall sys_io				; display AH
	mov ah, bl				; retrieve al
	mov al, 0
	syscall sys_io				; display AL

	pop bl
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; print 8bit decimal unsigned number
; input number in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_u8d:
	push a
	push b

	mov ah, 0
	mov b, 100
	div a, b
	push b			; save remainder
	cmp al, 0
	je skip100
	add al, $30
	mov ah, al
	mov al, 0
	syscall sys_io	; print coeff
skip100:
	pop a
	mov ah, 0
	mov b, 10
	div a, b
	push b			; save remainder
	cmp al, 0
	je skip10
	add al, $30
	mov ah, al
	mov al, 0
	syscall sys_io	; print coeff
skip10:
	pop a
	mov al, bl
	add al, $30
	mov ah, al
	mov al, 0
	syscall sys_io	; print coeff
	pop b
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INPUT 8BIT HEX INTEGER
; read 8bit integer into AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scan_u8x:
	enter 4
	push b
	push d

	lea d, [bp + -3]
	call gets				; get number

	mov bl, [d]
	mov bh, bl
	mov bl, [d + 1]
	call atoi				; convert to int in AL

	pop d
	pop b
	leave
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; input decimal number
; result in A
; 655'\0'
; low--------high
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scan_u16d:
	enter 8
	push si
	push b
	push c
	push d
	lea d, [bp +- 7]
	call gets
	call strlen			; get string length in C
	dec c
	mov si, d
	mov a, c
	shl a
	mov d, table_power
	add d, a
	mov c, 0
mul_loop:
	lodsb			; load ASCII to al
	cmp al, 0
	je mul_exit
	sub al, $30		; make into integer
	mov ah, 0
	mov b, [d]
	mul a, b			; result in B since it fits in 16bits
	mov a, b
	mov b, c
	add a, b
	mov c, a
	sub d, 2
	jmp mul_loop
mul_exit:
	mov a, c
	pop d
	pop c
	pop b
	pop si
	leave
	ret
