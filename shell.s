;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SHELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MEMORY MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 0000		ROM BEGIN
; ....
; 7FFF		ROM END
;
; 8000		RAM begin
; ....
; F7FF		Stack root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FF80		UART 0		(16550)
; FF90		UART 1		(16550)
; FFA0		RTC			(M48T02)
; FFB0		PIO 0		(8255)
; FFC0		PIO 1		(8255)
; FFD0		IDE			(Compact Flash / PATA)
; FFE0		Timer		(8253)
; FFF0		BIOS CONFIGURATION NV-RAM STORE AREA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SYSTEM CONSTANTS / EQUATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
STACK_BEGIN	.equ $F7FF	; beginning of stack
NULL		.equ 0
	
.export shell_transient_area
; declare RESET VECTOR. we must declare the reset vector of every new process
; so that the kernel knows where to jump when it loads the process

.dw SHELL_RESET_VECTOR

; file includes. these are functions used by the shell
.include "kernel.exp"
.include "stdio.s"
.include "ctype.s"
.include "token.s"

SHELL_RESET_VECTOR:	
	mov bp, STACK_BEGIN
	mov sp, STACK_BEGIN
	mov d, s_welcome
	call puts
	call cmd_printdate
shell_L0:
	sti
	mov byte [tokstr], 0			; clear tokstr (so that enter doesnt repeat last shell command)
	mov al, 18
	syscall sys_fileio				; print current path
	mov d, s_hash
	call puts
	call command_parser
	jmp shell_L0

command_parser:
	mov d, shell_input_buff
	mov a, shell_input_buff
	mov [prog], a			; reset tokenizer buffer pointer
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
parser_retry:
	call get_token
	mov al, [tok]
	cmp al, TOK_SEMI
	je parser_newtoken
	call putback
	ret
parser_cmd_not_found:
	call putback
	call cmd_exec			; execute as file/program
	jmp parser_retry		; check for more commands
	ret

parser_index: .dw 0

cmd_ps:
	call printnl
	syscall sys_list
	ret

cmd_fork:
	call printnl
	syscall sys_fork
	ret

cmd_fwb:
	syscall sys_fwb
	ret
	
cmd_fwk:
	syscall sys_fwk
	ret
	
loadcall:
	call get_token
	mov d, tokstr
	call strtoint
	
	mov [addr1], a			; save address
	mov d, s_dataentry
	call puts
	mov di, a					; save destiny
	call _load_hex
	call printnl

	mov a, [addr1]			; retrieve address
	
	call a
	ret
	
addr1: .dw 0
		
;******************************************************************************

	
; ************************************************************
; GET HEX FILE
; di = destination address
; return length in bytes in C
; ************************************************************
_load_hex:
	push bp
	mov bp, sp
	push a
	push b
	push d
	push si
	push di
	sub sp, $6000				; string data block
	mov c, 0
	
	mov a, sp
	inc a
	mov d, a				; start of string data block
	call gets				; get program string
	mov si, a
__load_hex_loop:
	lodsb					; load from [SI] to AL
	cmp al, 0				; check if ASCII 0
	jz __load_hex_ret
	mov bh, al
	lodsb
	mov bl, al
	call atoi				; convert ASCII byte in B to int (to AL)
	stosb					; store AL to [DI]
	inc c
	jmp __load_hex_loop
__load_hex_ret:
	add sp, $6000
	pop di
	pop si
	pop d
	pop b
	pop a
	mov sp, bp
	pop bp
	ret

mem_dump:
	call get_token
	mov d, tokstr
	call strtoint
	mov d, a				; dump pointer in d
	mov c, 0
dump_loop:
	mov al, cl
	and al, $0F
	jz print_base
back:
	mov al, [d]				; read byte
	mov bl, al
	call print_u8x
	mov a, $2000
	syscall sys_io			; space
	mov al, cl
	and al, $0F
	cmp al, $0F
	je print_ascii
back1:
	inc d
	inc c
	cmp c, 512
	jne dump_loop
	
	mov a, $0A00
	syscall sys_io
	mov a, $0D00
	syscall sys_io
	;call printnl
	ret
print_ascii:
	sub d, 16
	mov b, 16
print_ascii_L:
	inc d
	mov al, [d]				; read byte
	cmp al, $20
	jlu dot
	cmp al, $7E
	jleu ascii
dot:
	mov a, $2E00
	syscall sys_io
	jmp ascii_continue
ascii:
	mov ah, al
	mov al, 0
	syscall sys_io
ascii_continue:
	loopb print_ascii_L
	jmp back1
print_base:
	mov a, $0A00
	syscall sys_io
	mov a, $0D00
	syscall sys_io
	mov b, d
	call print_u16x				; display row
	mov a, $2000
	syscall sys_io
	jmp back

	
; ********************************************************************
; DATETIME
; ********************************************************************
cmd_printdate:
	mov al, 0			; print datetime
	syscall sys_datetime
	ret
	
cmd_setdate:
	mov al, 1			; set datetime
	syscall sys_datetime	
	ret	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE SYSTEM DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; infor for : IDE SERVICES INTERRUPT
; al = option
; IDE read/write sector
; 512 bytes
; user buffer pointer in D
; AH = number of sectors
; CB = LBA bytes 3..0	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE SYSTEM DATA STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for a directory we have the header first, followed by metadata
; header 1 sector (512 bytes)
; metadata 1 sector (512 bytes)
; HEADER ENTRIES:
; filename (64)
; parent dir LBA (2) -  to be used for faster backwards navigation...
;
; metadata entries:
; filename (24)
; attributes (1)
; LBA (2)
; size (2)
; day (1)
; month (1)
; year (1)
; packet size = 32 bytes
;
; first directory on disk is the root directory '/'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE SYSTEM DISK FORMATTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writes FST_TOTAL_SECTORS + FS_NBR_FILES disk sectors  with 0's
; this is the file system table formating
cmd_mkfs:
	mov al, 0
	syscall sys_fileio
	ret

cmd_fs_space:
	mov al, 1
	syscall sys_fileio
	call printnl
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE NEW DIRECTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search list for NULL name entry.
; add new directory to list
cmd_mkdir:
cmd_mkdir_L0:
	call get_token
	mov al, [toktyp]
	cmp al, TOKTYP_IDENTIFIER
	jne cmd_mkdir_end
; execute mkdir command
	mov d, tokstr
	mov al, 2
	syscall sys_fileio
	jmp cmd_mkdir_L0
cmd_mkdir_end:
	call putback		; if token was not an identifier, then put it back
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse path
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search for given directory inside current dir
; if found, read its LBA, and switch directories
; example: cd /usr/bin
cmd_cd:
	mov a, [prog]
	mov d, a
	mov al, 19
	syscall sys_fileio	; get dirID in A
	mov b, a
	mov al, 3
	syscall sys_fileio	; set dir to B
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_ls:	
	mov al, 4
	syscall sys_fileio
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pad string to 32 chars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; count in C
padding:
	push a
	mov a, 32
	mov b, c
	sub a, b
	mov c, a
padding_L1:
	mov ah, $20
	call putchar
	loopc padding_L1
	pop a
	ret
; file structure:
; 512 bytes header
; header used to tell whether the block is free


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE NEW BINARY FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search for first null block
cmd_mkbin:
	call get_token
	mov d, tokstr
	mov al, 6
	syscall sys_fileio
	ret

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PWD - PRINT WORKING DIRECTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
cmd_pwd:
	mov al, 7
	syscall sys_fileio
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
cmd_cat:
	call get_token
	mov al, [tok]
	cmp al, TOK_ANGLE
	je cmd_cat_write
cmd_cat_read:
	mov al, [toktyp]
	cmp al, TOKTYP_IDENTIFIER
	jne cmd_cat_end
	mov si, tokstr
	mov di, shell_transient_area
	call strcpy					; copy filename into transient area to pass to kernel
	mov d, shell_transient_area
	mov al, 8
	syscall sys_fileio				; read textfile into shell buffer
	mov d, shell_transient_area
	call puts					; print textfile to stdout
	call get_token
	jmp cmd_cat_read
cmd_cat_end:
	call putback
	call printnl
	ret
cmd_cat_write:
	call get_token
	mov si, tokstr
	mov di, shell_transient_area + 1
	call strcpy				; copy filename
	mov d, shell_transient_area + 512		; get text contents
	call gettxt
	mov d, shell_transient_area
	mov al, 5
	syscall sys_fileio
	call printnl
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RMDIR - remove DIR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deletes directory  entry in the current directory's file list 
; also deletes the actual directory entry in the FST
cmd_rmdir:
cmd_rmdir_L0:
	call get_token
	mov al, [toktyp]
	cmp al, TOKTYP_IDENTIFIER
	jne cmd_rmdir_end
; execute rmdir command
	mov d, tokstr
	mov al, 9
	syscall sys_fileio
	jmp cmd_rmdir_L0
cmd_rmdir_end:
	call putback		; if token was not an identifier, then put it back
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RM - remove file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; frees up the data sectors for the file further down the disk
; deletes file entry in the current directory's file list 
cmd_rm:
cmd_rm_L0:
	call get_token
	mov al, [toktyp]
	cmp al, TOKTYP_IDENTIFIER
	jne cmd_rm_end
; execute rm command
	mov d, tokstr
	mov al, 10
	syscall sys_fileio
	jmp cmd_rm_L0
cmd_rm_end:
	call putback		; if token was not an identifier, then put it back
	ret
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHMOD - change file permissions
;; ex: chmod 7 <filename>
;; 1 = exec, 2 = write, 4 = read
;; we only have one digit in Sol-1 for now since we don't have users or groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filename passed to the kernel in D
; permission value in A
cmd_chmod:
	call get_token				; read permission value
	mov d, tokstr			; pointer to permission token string
	call strtoint				; integer in A
	and al, %00000111			; mask out garbage
	mov bl, al					; save permission in bl
	call get_token				; get filename. D already points to tokstr
	mov al, 14
	syscall sys_fileio			; call kernel to set permission
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mv - move / change file name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_mv:
	call get_token
	mov si, tokstr
	mov di, shell_transient_area
	call strcpy
	
	call get_token
	mov si, tokstr
	mov di, shell_transient_area + 128
	call strcpy

	mov d, shell_transient_area
	mov al, 15	; mv command
	syscall sys_fileio
	
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXEC/OPEN PROGRAM/FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_exec:
	mov al, [tok]
	cmp al, TOK_END
	je cmd_exec_ret		; check for NULL input
	mov a, [prog]
	mov d, a
	mov al, 20
	mov di, shell_transient_area
	syscall sys_fileio	; success code in A
	cmp a, 0
	je cmd_exec_error
	call shell_transient_area
	ret
cmd_exec_error:
	mov d, s_command_notfound
	call puts
cmd_exec_ret:
	ret

cmd_more:
	
	ret

cmd_f_find:

	ret

cmd_shutdown:
	halt

cmd_whoami:
	mov d, s_root
	call puts
	ret

cmd_reboot:
	syscall sys_reboot

cmd_clear:
	mov d, s_telnet_clear
	call puts
	ret

s_telnet_clear:		.db 27, "[2J", 27, "[H", 0
s_welcome:			.db "\n\rWelcome to Sol-OS ver. 0.1\n\r", 0
s_dataentry:		.db "% ", 0
s_syntax_err:		.db "\n\rsyntax error\n\r", 0
s_command_notfound:	.db "command not found\n", 0
s_root:			.db "root\n", 0
s_hash:			.db " # ", 0

; shell variables
shell_input_buff:	.fill 512, 0
shell_buff_ptr:		.dw 0

keywords:
	.db "mkfs", 0
	.db "ps", 0
	.db "ls", 0
	.db "cd", 0
	.db "fwb", 0
	.db "fwk", 0
	.db "fss", 0
	.db "fork", 0
	.db "dmp", 0
	.db "lc", 0
	.db "cat", 0
	.db "rm", 0
	.db "mkbin", 0
	.db "mkdir", 0
	.db "rmdir", 0
	.db "chmod", 0
	.db "mv", 0
	.db "pwd", 0
	.db "date", 0
	.db "sdate", 0
	.db "reboot", 0
	.db "clear", 0
	.db "more", 0
	.db "find", 0
	.db "whoami", 0
	.db "shutdown", 0
	.db 0

keyword_pointers:
	.dw cmd_mkfs
	.dw cmd_ps
	.dw cmd_ls
	.dw cmd_cd
	.dw cmd_fwb
	.dw cmd_fwk
	.dw cmd_fs_space
	.dw cmd_fork
	.dw mem_dump
	.dw loadcall
	.dw cmd_cat
	.dw cmd_rm
	.dw cmd_mkbin
	.dw cmd_mkdir
	.dw cmd_rmdir
	.dw cmd_chmod
	.dw cmd_mv
	.dw cmd_pwd
	.dw cmd_printdate
	.dw cmd_setdate
	.dw cmd_reboot
	.dw cmd_clear
	.dw cmd_more
	.dw cmd_f_find
	.dw cmd_whoami
	.dw cmd_shutdown

shell_transient_area:	.db 0	; shell transient data area

.end
