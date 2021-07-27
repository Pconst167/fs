;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; string.s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strrev
; reverse a string
; D = string address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 01234
strrev:
	pusha
	call strlen	; length in C
	mov a, c
	cmp a, 1
	jleu strrev_end	; check string length. string len must be > 1
	dec a
	mov si, d	; beginning of string
	mov di, d	; beginning of string (for destinations)
	add d, a	; end of string
	mov a, c
	shr a		; divide by 2
	mov c, a	; C now counts the steps
strrev_L0:
	mov bl, [d]	; save load right-side char into BL
	lodsb		; load left-side char into AL; increase SI
	mov [d], al	; store left char into right side
	mov al, bl
	stosb		; store right-side char into left-side; increase DI
	dec c
	dec d
	cmp c, 0
	jne strrev_L0
strrev_end:
	popa
	ret
	
	
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strchr
; search string in D for char in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strchr:
strchr_L0:
	mov bl, [d]
	cmp bl, 0
	je strchr_end
	cmp al, bl
	je strchr_end
	inc d
	jmp strchr_L0
strchr_end:
	mov al, bl
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strstr
; find sub-string
; str1 in SI
; str2 in DI
; SI points to end of source string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strstr:
	push al
	push d
	push di
strstr_loop:
	cmpsb					; compare a byte of the strings
	jne strstr_ret
	lea d, [di + 0]
	mov al, [d]
	cmp al, 0				; check if at end of string (null)
	jne strstr_loop				; equal chars but not at end
strstr_ret:
	pop di
	pop d
	pop al
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; length of null terminated string
; result in C
; pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strlen:
	push d
	mov c, 0
strlen_L1:
	cmp byte [d], 0
	je strlen_ret
	inc d
	inc c
	jmp strlen_L1
strlen_ret:
	pop d
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STRCMP
; compare two strings
; str1 in SI
; str2 in DI
; CREATE A STRING COMPAIRON INSTRUCION ?????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strcmp:
	push al
	push d
	push di
	push si
strcmp_loop:
	cmpsb					; compare a byte of the strings
	jne strcmp_ret
	lea d, [si +- 1]
	mov al, [d]
	cmp al, 0				; check if at end of string (null)
	jne strcmp_loop				; equal chars but not at end
strcmp_ret:
	pop si
	pop di
	pop d
	pop al
	ret


; STRCPY
; copy null terminated string from SI to DI
; source in SI
; destination in DI
strcpy:
	push si
	push di
	push al
strcpy_L1:
	lodsb
	stosb
	cmp al, 0
	jne strcpy_L1
strcpy_end:
	pop al
	pop di
	pop si
	ret

; STRCAT
; concatenate a NULL terminated string into string at DI, from string at SI
; source in SI
; destination in DI
strcat:
	push si
	push di
	push a
	push d
	mov a, di
	mov d, a
strcat_goto_end_L1:
	mov al, [d]
	cmp al, 0
	je strcat_start
	inc d
	jmp strcat_goto_end_L1
strcat_start:
	mov di, d
strcat_L1:
	lodsb
	stosb
	cmp al, 0
	jne strcat_L1
strcat_end:
	pop d
	pop a
	pop di
	pop si
	ret
