;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ctype.s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C character classification is an operation provided by a group of functions in the ANSI C Standard Library
;; for the C programming language. These functions are used to test characters for membership in a particular
;; class of characters, such as alphabetic characters, control characters, etc. Both single-byte, and wide
;; characters are supported.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isalnum 
;; isalpha 
;; islower 
;; isupper 
;; isdigit 
;; isxdigit
;; iscntrl 
;; isgraph 
;; isspace 
;; isblank 
;; isprint 
;; ispunct 
;; tolower 
;; toupper


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS ALPHANUMERIC
;; sets ZF according with result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
isalnum:
	call isalpha
	je isalnum_exit
	call isdigit
isalnum_exit:
	ret	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS DIGIT
;; sets ZF according with result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
isdigit:
	mov g, a
	cmp al, '0'
	jlu isdigit_false
	cmp al, '9'
	jgu isdigit_false
	and al, 0	; set ZF
	mov a, g
	ret
isdigit_false:
	or al, 1	; clear ZF
	mov a, g
	ret	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS ALPHA
;; sets ZF according with result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
isalpha:
	mov g, a
	cmp al, '_'
	je isalpha_true
	cmp al, '.'
	je isalpha_true
	cmp al, 'A'
	jlu isalpha_false
	cmp al, 'z'
	jgu isalpha_false
	cmp al, 'Z'
	jleu isalpha_true
	cmp al, 'a'
	jgeu isalpha_true
isalpha_false:
	or al, 1	; clear ZF
	mov a, g
	ret
isalpha_true:
	and al, 0	; set ZF
	mov a, g
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS SPACE
;; sets ZF according with result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
isspace:
	cmp al, $20		; ' '
	je isspace_exit
	cmp al, $09		; '\t'
	je isspace_exit
	cmp al, $0A		; '\n'
	je isspace_exit
	cmp al, $0D		; '\r'
	je isspace_exit
	cmp al, $0B		; '\v'
isspace_exit:
	ret	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TO LOWER
; input in AL
; output in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to_lower:
	cmp al, 'Z'
	jgu to_lower_ret
	add al, $20				; convert to lower case
to_lower_ret:
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TO UPPER
; input in AL
; output in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to_upper:
	cmp al, 'a'
	jlu to_upper_ret
	sub al, $20			; convert to upper case
to_upper_ret:
	ret

