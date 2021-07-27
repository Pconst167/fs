TOKTYP_IDENTIFIER	.equ 0
TOKTYP_KEYWORD		.equ 1
TOKTYP_DELIMITER	.equ 2
TOKTYP_STRING		.equ 3
TOKTYP_CHAR			.equ 4
TOKTYP_NUMERIC		.equ 5
TOKTYP_END			.equ 6

TOK_NULL			.equ 0
TOK_FSLASH			.equ 1
TOK_TIMES 			.equ 2
TOK_PLUS 			.equ 3
TOK_MINUS 			.equ 4
TOK_DOT				.equ 5
TOK_DDOT			.equ 6
TOK_SEMI			.equ 7
TOK_ANGLE			.equ 8
TOK_END				.equ 9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; token parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_token:
	push a
	push d
	mov al, TOK_NULL
	mov [tok], al				; nullify token
	mov a, [prog]
	mov si, a
	mov di, tokstr
skip_spaces:
	lodsb
	call isspace
	je skip_spaces
	cmp al, 0			; check for end of input (NULL)
	je get_token_end
get_tok_type:
	call isdigit
	jz is_numeric
	call isalpha
	jz is_alphanumeric
; other token types
get_token_slash:
	cmp al, '/'				; check if '/'
	jne get_token_dash
	stosb					; store '/' into token string
	mov al, 0
	stosb					; terminate token string
	mov al, TOK_FSLASH
	mov [tok], al			
	mov al, TOKTYP_DELIMITER
	mov [toktyp], al
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
get_token_dash:
	cmp al, '-'				; check if '-'
	jne get_token_semi
	stosb					; store '-' into token string
	mov al, 0
	stosb					; terminate token string
	mov al, TOK_MINUS
	mov [tok], al			
	mov al, TOKTYP_DELIMITER
	mov [toktyp], al
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
get_token_semi:
	cmp al, $3B				; check if ';'
	jne get_token_angle
	stosb					; store ';' into token string
	mov al, 0
	stosb					; terminate token string
	mov al, TOK_SEMI
	mov [tok], al			
	mov al, TOKTYP_DELIMITER
	mov [toktyp], al
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
get_token_angle:
	cmp al, $3E				; check if '>'
	jne get_token_skip
	stosb					; store ';' into token string
	mov al, 0
	stosb					; terminate token string
	mov al, TOK_ANGLE
	mov [tok], al			
	mov al, TOKTYP_DELIMITER
	mov [toktyp], al
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
get_token_skip:
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
get_token_end:				; end of file token
	mov al, TOK_END
	mov [tok], al
	mov al, TOKTYP_END
	mov [toktyp], al
	jmp get_token_return
is_numeric:
	stosb
	lodsb
	call isdigit			;check if is numeric
	jz is_numeric
	mov al, 0
	stosb
	mov al, TOKTYP_NUMERIC
	mov [toktyp], al
	sub si, 1
	mov a, si
	mov [prog], a		; update pointer
	jmp get_token_return
is_alphanumeric:
	stosb
	lodsb
	call isalnum			;check if is alphanumeric
	jz is_alphanumeric
	cmp al, $2E				; check if is '.'
	je is_alphanumeric
	mov al, 0
	stosb
	mov al, TOKTYP_IDENTIFIER
	mov [toktyp], al
	sub si, 1
	mov a, si
	mov [prog], a		; update pointer
get_token_return:
	pop d
	pop a
	ret
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT BACK TOKEN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
putback:
	push a
	push si
	mov si, tokstr	
putback_loop:
	lodsb
	cmp al, 0
	je putback_end
	mov a, [prog]
	dec a
	mov [prog], a			; update pointer
	jmp putback_loop
putback_end:
	pop si
	pop a
	ret


prog:		.dw 0			; pointer to current position in buffer

toktyp: 	.db 0			; token type symbol
tok:		.db 0			; current token symbol
tokstr:		.fill 256, 0	; token as a string
