;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ed - the original Unix text editor!
;
; paulo constantino - 2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.include "kernel.exp"
.include "shell.exp"

.org shell_transient_area

main:
main_L0:
	sti
	mov byte [tokstr], 0			; clear tokstr (so that enter doesnt repeat last shell command)
	call command_parser

	mov al, [quit_flag]
	cmp al, 1
	je ed_quit
	jmp main_L0

ed_quit:
	ret

command_parser:
	mov d, input_buff
	mov a, d
	mov [prog], a
	call gets						; get command
parser_newtoken:
	call get_token					; get command into tokstr
	

	mov di, keywords
	mov a, 0
	mov [parser_index], a		; reset keywords index
parser_L2:
	mov si, tokstr
	call strcmp
	je parser_cmd_equal
parser_L2_L0:
	lea d, [di + 0]
	mov al, [d]
	cmp al, 0
	je parser_L2_L0_exit			; run through the keyword until finding NULL
	add di, 1
	jmp parser_L2_L0
parser_L2_L0_exit:
	add di, 1				; then skip NULL byte at the end 
	mov a, [parser_index]
	add a, 2
	mov [parser_index], a			; increase keywords table index
	lea d, [di + 0]
	mov al, [d]
	cmp al, 0
	je parser_cmd_not_found
	jmp parser_L2
parser_cmd_equal:
	mov a, $0D00
	syscall sys_io				; print carriage return
	mov a, [parser_index]			; get the keyword pointer
	call [a + keyword_pointers]		; execute command
	mov a, $0D00
	syscall sys_io				; print carriage return
	ret
parser_cmd_not_found:
	mov ah, '?'
	call putchar
	ret

parser_index: .dw 0

cmd_append:
	mov a, [txt_buffer_ptr]
	mov d, a
cmd_append_L0:
	call gets		; read new line
	mov si, d
	mov di, s_dot
	call strcmp
	je cmd_append_end
	mov a, [txt_buffer_ptr]
	mov si, a
cmd_append_L1:		; look for NULL termination
	lodsb
	cmp al, 0
	jne cmd_append_L1
	lea d, [si + -1]
	mov al, $0A
	mov [d], al
	lea d, [si + 0]
	mov al, 0
	mov [d], al
	mov a, d
	mov [txt_buffer_ptr], a
	jmp cmd_append_L0
cmd_append_end:
	mov al, 0
	mov [d], al
	ret

cmd_insert:
	
	ret


cmd_quit:
	mov al, 1
	mov [quit_flag], al
	ret

cmd_edit:
	call get_token			; read filename
	mov si, tokstr
	mov di, text_buffer
	call strcpy				; copy filename
	mov d, text_buffer		; pointer to write buffer
	mov al, 8
	syscall sys_fileio		; read textfile into buffer
	mov d, text_buffer
	call strlen
	mov a, c				; find size of buffer
	add a, text_buffer
	mov d, a
	mov al, $0A
	mov [d], al
	inc d
	mov al, 0
	mov [d], al
	mov a, d
	mov [txt_buffer_ptr], a	; set buffer pointer

	mov d, text_buffer
	call strlen
	mov a, c
	call print_u16d
	call printnl
	ret

cmd_write:
	call get_token		; read filename
	mov si, tokstr
	mov di, transient_data + 1
	call strcpy				; copy filename

	mov d, transient_data	; pass data to kernel. starting at 512 byte header. text_buffer follows the header in mem.
	mov al, 5
	syscall sys_fileio

	mov d, text_buffer
	call strlen
	mov a, c
	call print_u16d
	call printnl
	ret
	
cmd_print:
	mov d, text_buffer
	call puts
	ret

input_buff:			.fill 512, 0

quit_flag:			.db 0

txt_buffer_ptr:		.dw text_buffer

s_dot:				.db ".", 0

keywords:
	.db "a", 0
	.db "p", 0
	.db "e", 0
	.db "w", 0
	.db "q", 0
	.db 0

keyword_pointers:
	.dw cmd_append
	.dw cmd_print
	.dw cmd_edit
	.dw cmd_write
	.dw cmd_quit

s_bad_command:		.db "?", 0

; file includes. these are functions used by the shell
.include "stdio.s"
.include "ctype.s"
.include "token.s"

transient_data:	.fill 512
text_buffer:	.db 0			

.end

