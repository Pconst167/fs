;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; KERNEL
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
; FFA0		RTC		(M48T02)
; FFB0		PIO 0		(8255)
; FFC0		PIO 1		(8255)
; FFD0		IDE		(Compact Flash / PATA)
; FFE0		Timer		(8253)
; FFF0		BIOS CONFIGURATION NV-RAM STORE AREA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SYSTEM CONSTANTS / EQUATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_UART0_DATA			.equ $FF80				; data
_UART0_DLAB_0		.equ $FF80				; divisor latch low byte
_UART0_DLAB_1		.equ $FF81				; divisor latch high byte
_UART0_IER			.equ $FF81				; Interrupt enable register
_UART0_FCR			.equ $FF82				; FIFO control register
_UART0_LCR			.equ $FF83				; line control register
_UART0_LSR			.equ $FF85				; line status register

_IDE_BASE			.equ $FFD0				; IDE BASE
_IDE_R0				.equ _IDE_BASE + 0		; DATA PORT
_IDE_R1				.equ _IDE_BASE + 1		; READ: ERROR CODE, WRITE: FEATURE
_IDE_R2				.equ _IDE_BASE + 2		; NUMBER OF SECTORS TO TRANSFER
_IDE_R3				.equ _IDE_BASE + 3		; SECTOR ADDRESS LBA 0 [0:7]
_IDE_R4				.equ _IDE_BASE + 4		; SECTOR ADDRESS LBA 1 [8:15]
_IDE_R5				.equ _IDE_BASE + 5		; SECTOR ADDRESS LBA 2 [16:23]
_IDE_R6				.equ _IDE_BASE + 6		; SECTOR ADDRESS LBA 3 [24:27 (LSB)]
_IDE_R7				.equ _IDE_BASE + 7		; READ: STATUS, WRITE: COMMAND

_7SEG_DISPLAY		.equ $FFB0				; BIOS POST CODE HEX DISPLAY (2 DIGITS)
_BIOS_POST_CTRL		.equ $FFB3				; BIOS POST DISPLAY CONTROL REGISTER, 80h = As Output
_PIO_A				.equ $FFB0		
_PIO_B				.equ $FFB1
_PIO_C				.equ $FFB2
_PIO_CONTROL		.equ $FFB3				; PIO CONTROL PORT

_TIMER_C_0			.equ $FFE0				; TIMER COUNTER 0
_TIMER_C_1			.equ $FFE1				; TIMER COUNTER 1
_TIMER_C_2			.equ $FFE2				; TIMER COUNTER 2
_TIMER_CTRL			.equ $FFE3				; TIMER CONTROL REGISTER

STACK_BEGIN		.equ $F7FF				; beginning of stack
FIFO_SIZE			.equ (1024*2)

_NULL				.equ 0



; boot-sector(1) | kernel-sectors(32) | inode-bitmap | rawdata-bitmap | inode-table | raw-disk-data
;
; inode-table format:
;	file-type(f, d)
;	permissons
;	link-count
;	filesize
;	time-stamps
;	15 data block pointers
;	single-indirect pointer
;

FST_ENTRY_SIZE			.equ 32
FST_FILES_PER_SECT		.equ (512 / FST_ENTRY_SIZE)
FST_FILES_PER_DIR		.equ 16
FST_NBR_DIRECTORIES		.equ 64
						; 1 sector for header, the rest is for the list of files/dirs
FST_SECTORS_PER_DIR		.equ (1 + (FST_ENTRY_SIZE * FST_FILES_PER_DIR / 512))		
FST_TOTAL_SECTORS		.equ (FST_SECTORS_PER_DIR * FST_NBR_DIRECTORIES)
FST_LBA_START			.equ 32
FST_LBA_END				.equ (FST_LBA_START + FST_TOTAL_SECTORS - 1)

FS_NBR_FILES 			.equ (FST_NBR_DIRECTORIES * FST_FILES_PER_DIR)
FS_SECTORS_PER_FILE		.equ 32				; the first sector is always a header with a NULL parameter (first byte)
											; so that we know which blocks are free or taken
FS_FILE_SIZE			.equ (FS_SECTORS_PER_FILE * 512)									
FS_TOTAL_SECTORS		.equ (FS_NBR_FILES * FS_SECTORS_PER_FILE)
FS_LBA_START			.equ (FST_LBA_END + 1)
FS_LBA_END				.equ (FS_LBA_START + FS_NBR_FILES - 1)

CF_CARD_LBA_SIZE		.equ $800			; temporary small size

ROOT_dirID:				.equ FST_LBA_START


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL SYSTEM VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXTERNAL INTERRUPT TABLE
; highest priority at lowest address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.dw INT_0
.dw INT_1
.dw INT_2
.dw INT_3
.dw INT_4
.dw INT_5
.dw INT_6
.dw INT_7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESET VECTOR DECLARATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.dw KERNEL_RESET_VECTOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXCEPTION VECTOR TABLE
;; total of 7 entries, starting at address $0012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.dw TRAP_PRIVILEGE
.dw TRAP_DIV_ZERO
.dw UNDEFINED_OPCODE
.dw _NULL
.dw _NULL
.dw _NULL
.dw _NULL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM CALL VECTOR TABLE
;; starts at address $0020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.dw TRAP_BREAKPOINT
.dw RTC_SERVICES						
.dw IDE_SERVICES_KERNEL
.dw io_services
.dw file_system
.dw cmd_fork
.dw cmd_fwb
.dw cmd_fwk
.dw list_procs
.dw DATETIME_SERVICES
.dw syscall_reboot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE INCLUDES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.include "bios.exp"	; to obtain the boot_org location (for reboots)
.include "stdio.s"
.include "ctype.s"
.include "token.s"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sys_bkpt		.equ 0
sys_rtc			.equ 1
sys_ide			.equ 2
sys_io			.equ 3
sys_fileio		.equ 4
sys_fork		.equ 5
sys_fwb			.equ 6
sys_fwk			.equ 7
sys_list		.equ 8
sys_datetime	.equ 9
sys_reboot		.equ 10

.export sys_bkpt
.export sys_ide
.export sys_io
.export sys_fileio
.export sys_fork
.export sys_list
.export sys_rtc
.export sys_fwb
.export sys_fwk
.export sys_datetime
.export sys_reboot

.export transient_area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXTERNAL INTERRUPTS' CODE BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; uart
INT_0:
	sysret
INT_1:
	sysret
INT_2:
	sysret
INT_3:
	sysret
INT_4:
	sysret
INT_5:
	sysret
INT_6:	
; save all registers into kernel stack
	pusha
	mov ah, 0
	mov al, [active_proc_index]
	shl a							; x2
	mov a, [PROC_TABLE_convert + a]	; get process state start index
		
	mov di, a
	mov a, sp
	inc a
	mov si, a
	mov c, 20
	rep movsb					; save process state!
; restore kernel stack position to point before interrupt arrived
	add sp, 20
; now load next process in queue
	mov al, [active_proc_index]
	mov bl, [nbr_active_procs]
	cmp al, bl
	je INT6_cycle_back
	inc al						; next process is next in the series
	jmp INT6_continue
INT6_cycle_back:
	mov al, 1				; next process = process 1
INT6_continue:
	mov [active_proc_index], al		; set next active proc

; calculate LUT entry for next process
	mov ah, 0
	shl a							; x2
	mov a, [PROC_TABLE_convert + a]		; get process state start index	
	
	mov si, a						; source is proc state block
	mov a, sp
	sub a, 19
	mov di, a						; destination is kernel stack
; restore SP
	dec a
	mov sp, a
	mov c, 20
	rep movsb
; set VM process
	mov al, [active_proc_index]
	setptb
		
	mov byte[_TIMER_C_0], 0				; load counter 0 low byte
	mov byte[_TIMER_C_0], $10				; load counter 0 high byte
			
	popa
	sysret

INT_7:
	push a
	push d
	pushf
			
	mov a, [fifo_pi]
	mov d, a
				
	mov al, [_UART0_DATA]			; get character
	mov [d], al					; add to fifo
	
	mov a, [fifo_pi]
	inc a
	cmp a, fifo + FIFO_SIZE 				; check if pointer reached the end of the fifo
	jne INT_7_continue
	mov a, fifo	
INT_7_continue:	
	mov [fifo_pi], a			; update fifo pointer
	
	popf
	pop d
	pop a	
	sysret
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; REBOOT SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
syscall_reboot:
	mov d, s_rebooting
	call puts
	mov a, boot_origin		; set address to bootloader origin
	paging_off				; turn paging OFF and jump to bootloader

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXCEPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
list_procs:
	mov d, proc_availability_table + 1
	mov c, 1
list_procs_L0:	
	mov al, [d]
	cmp al, 1
	jne list_procs_next
	mov b, d
	sub b, proc_availability_table
	shl b, 5
	push d
	mov a, c
	call print_u8d
	mov ah, ':'
	call putchar
	mov ah, ' '
	call putchar
	mov d, b
	add d, proc_names
	call puts
	call printnl
	pop d
list_procs_next:
	inc d
	inc c
	cmp c, 9
	jne list_procs_L0
list_procs_end:
	sysret

; list_procs:
	; mov d, proc_names + 32
; list_procs_L0:	
	; mov al, [d]
	; cmp al, 0
	; je list_procs_end
	; call puts
	; call printnl
	; add d, 32
	; jmp list_procs_L0
; list_procs_end:
	; sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRIVILEGE EXCEPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TRAP_PRIVILEGE:
	push d

	mov d, s_priviledge
	call puts

	pop d
							; enable interrupts
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BREAKPOINT EXCEPTION
; IMPORTANT: values in the stack are being pushed in big endian. i.e.: MSB at low address
; and LSB at high address. *** NEED TO CORRECT THIS IN THE MICROCODE and make it little endian again ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TRAP_BREAKPOINT:
	pusha
	mov d, s_break1
	call puts
trap_break_prompt:
	call printnl
	call scan_u16d
	cmp a, 0
	je trap_break_regs
	cmp a, 1
	je trap_break_mem
trap_break_end:	
	popa
	sysret
trap_break_regs:
	mov a, sp
	add a, 14	; back-track 7 registers
	mov d, a
	mov cl, 7
trap_regs_L0:
	mov b, [d]
	swp b
	call print_u16x	; print register value
	call printnl
	sub d, 2
	sub cl, 1
	cmp cl, 0
	jne trap_regs_L0
	jmp trap_break_prompt
trap_break_continue:
	call printnl
	jmp trap_break_end
trap_break_mem:
	call printnl
	call scan_u16x
	mov si, a			; data source from user space
	mov di, transient_area - 512		; destination in kernel space
	mov c, 256
	load				; transfer data to kernel space!
	mov d, transient_area - 512		; dump pointer in d
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
	cmp c, 256
	jne dump_loop
	call printnl
	jmp trap_break_end	; go to trap_breakpoint return point
print_ascii:
	mov a, $2000
	syscall sys_io
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
	call printnl
	mov b, d
	call print_u16x				; display row
	mov a, $3A00
	syscall sys_io
	mov a, $2000
	syscall sys_io
	jmp back

s_break1:	.db "\n\rDebugger entry point.\n\r"
			.db "0. Show Registers\n\r"
			.db "1. Show 256-byte RAM block\n\r"
			.db "2. Continue Execution", 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DIVIDE BY ZERO EXCEPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TRAP_DIV_ZERO:
	push a
	push d
	pushf
		
	mov d, s_divzero
	call puts
	
	popf
	pop d
	pop a
							; enable interrupts
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNDEFINED OPCODE EXCEPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UNDEFINED_OPCODE:
	sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RTC SERVICES INTERRUPT
; RTC I/O bank = FFA0 to FFAF
; FFA0 to FFA7 is scratch RAM
; control register at $FFA8 [ W | R | S | Cal4..Cal0 ]
; al = 0..6 -> get
; al = 7..D -> set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RTC_SERVICES:
	push al
	push d
	cmp al, 6
	jgu RTC_SET
RTC_GET:
	add al, $A9			; generate RTC address to get to address A9 of clock
	mov ah, $FF		
	mov d, a				; get to FFA9 + offset
	mov byte[$FFA8], $40		; set R bit to 1
	mov al, [d]			; get data
	mov byte[$FFA8], 0		; reset R bit
	mov ah, al
	pop d
	pop al
	sysret
RTC_SET:
	push bl
	mov bl, ah		; set data asIDE
	add al, $A2		; generate RTC address to get to address A9 of clock
	mov ah, $FF		
	mov d, a		; get to FFA9 + offset
	mov al, bl		; get data back
	mov byte[$FFA8], $80	; set W bit to 1
	mov [d], al		; set data
	mov byte[$FFA8], 0		; reset write bit
	pop bl
	pop d
	pop al
	sysret

datetime_serv_tbl:
	.dw print_date
	.dw set_date
DATETIME_SERVICES:
	jmp [datetime_serv_tbl + al]			
print_date:
	mov a, $0D00				; print carriage return char
	mov al, 3
	syscall sys_rtc				; get week
	mov al, ah
	mov ah, 0
	shl a, 2					; times 16
	mov d, s_week
	add d, a
	call puts
	mov a, $2000
	syscall sys_io					; display ' '
	
	mov al, 4
	syscall sys_rtc					; get day
	mov bl, ah
	call print_u8x
	mov a, $2000
	syscall sys_io					; display ' '
	
	mov al, 05
	syscall sys_rtc				; get month
	mov al, ah
	mov ah, 0
	shl a, 2					; times 16
	mov d, s_months
	add d, a
	call puts
	
	mov a, $2000
	syscall sys_io			; display ' '
	
	mov bl, $20
	call print_u8x			; print 20 for year prefix
	mov al, 06
	syscall sys_rtc					; get year
	mov bl, ah
	call print_u8x
	
	mov a, $2000	
	syscall sys_io			; display ' '

	mov al, 2
	syscall sys_rtc					; get hours
	mov bl, ah
	call print_u8x
	mov a, $3A00		
	syscall sys_io				; display ':'

	mov al, 01
	syscall sys_rtc					; get minutes
	mov bl, ah
	call print_u8x
	mov a, $3A00	
	syscall sys_io			; display ':'

	mov al, 0
	syscall sys_rtc					; get seconds
	mov bl, ah
	call print_u8x
	
	call printnl
	sysret
	
set_date:
	mov d, s_set_year
	call puts
	call scan_u8x				; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 0Dh				; set RTC year
	syscall sys_rtc					; set RTC
	
	mov d, s_set_month
	call puts
	call scan_u8x					; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 0Ch				; set RTC month
	syscall sys_rtc					; set RTC

	mov d, s_set_day
	call puts
	call scan_u8x					; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 0Bh				; set RTC month
	syscall sys_rtc					; set RTC

	mov d, s_set_week
	call puts
	call scan_u8x					; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 0Ah				; set RTC month
	syscall sys_rtc					; set RTC

	mov d, s_set_hours
	call puts
	call scan_u8x					; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 09h				; set RTC month
	syscall sys_rtc					; set RTC

	mov d, s_set_minutes
	call puts
	call scan_u8x					; read integer into A
	shl a, 8				; only AL used, move to AH
	mov al, 08h				; set RTC month
	syscall sys_rtc					; set RTC

	mov d, s_set_seconds
	call puts
	call scan_u8x					; read integer into A
	shl a, 8					; only AL used, move to AH
	mov al, 07h				; set RTC month
	syscall sys_rtc					; set RTC
	sysret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDE SERVICES INTERRUPT
; al = option
; 0 = ide reset, 1 = ide sleep, 2 = read sector, 3 = write sector
; IDE read/write sector
; 512 bytes
; user buffer pointer in D
; AH = number of sectors
; CB = LBA bytes 3..0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ide_serv_tbl:
	.dw IDE_RESET
	.dw IDE_SLEEP
	.dw IDE_READ_SECT
	.dw IDE_WRITE_SECT
IDE_SERVICES_KERNEL:
	jmp [ide_serv_tbl + al]			
IDE_RESET:			
	mov byte[_IDE_R7], 4		; RESET IDE
	call IDE_wait				; wait for IDE ready			 			
	mov byte[_IDE_R6], $E0		; LBA3= 0, MASTER, MODE= LBA				
	mov byte[_IDE_R1], 1		; 8-BIT TRANSFERS			
	mov byte[_IDE_R7], $EF		; SET FEATURE COMMAND
	sysret
IDE_SLEEP:
	call IDE_wait					; wait for IDE ready			 			
	mov byte [_IDE_R6], %01000000	; lba[3:0](reserved), bit 6=1
	mov byte [_IDE_R7], $E6		; sleep command
	call IDE_wait					; wait for IDE ready
	sysret
IDE_READ_SECT:
	mov al, ah
	mov ah, bl
	mov [_IDE_R2], a			; number of sectors (0..255)
	mov al, bh
	mov [_IDE_R4], al
	mov a, c
	mov [_IDE_R5], al
	mov al, ah
	and al, %00001111
	or al, %11100000			; mode lba, master
	mov [_IDE_R6], al
IDE_READ_SECT_wait:
	mov al, [_IDE_R7]	
	and al, $80				; BUSY FLAG
	jnz IDE_READ_SECT_wait
	mov al, $20
	mov [_IDE_R7], al			; read sector cmd
	call IDE_read	
	sysret
IDE_WRITE_SECT:
	mov al, ah
	mov ah, bl
	mov [_IDE_R2], a			; number of sectors (0..255)
	mov al, bh
	mov [_IDE_R4], al
	mov a, c
	mov [_IDE_R5], al
	mov al, ah
	and al, %00001111
	or al, %11100000			; mode lba, master
	mov [_IDE_R6], al
IDE_WRITE_SECT_wait:
	mov al, [_IDE_R7]	
	and al, $80				; BUSY FLAG
	jnz IDE_WRITE_SECT_wait
	mov al, $30
	mov [_IDE_R7], al			; write sector cmd
	call IDE_write			
	sysret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; READ IDE DATA
; pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IDE_read:
	push d
IDE_read_loop:
	mov al, [_IDE_R7]	
	and al, 80h				; BUSY FLAG
	jnz IDE_read_loop			; wait loop
	
	mov al, [_IDE_R7]
	and al, %00001000			; DRQ FLAG
	jz IDE_read_end
	mov al, [_IDE_R0]
	mov [d], al
	inc d
	jmp IDE_read_loop
IDE_read_end:
	pop d
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE IDE DATA
; data pointer in D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IDE_write:
	push d
IDE_write_loop:
	mov al, [_IDE_R7]	
	and al, 80h				; BUSY FLAG
	jnz IDE_write_loop			; wait loop
	
	mov al, [_IDE_R7]
	and al, %00001000			; DRQ FLAG
	jz IDE_write_end
	mov al, [d]
	mov [_IDE_R0], al
	inc d 
	jmp IDE_write_loop
IDE_write_end:
	pop d
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; wait for IDE to be ready
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IDE_wait:
	mov al, [_IDE_R7]	
	and al, 80h				; BUSY FLAG
	jnz IDE_wait
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; i/o interrupt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
io_services_jmp:
	.dw io_putchar
	.dw io_getchar
	.dw io_uart_init
io_services:
	jmp [io_services_jmp + al]
io_uart_init:
	mov byte[_UART0_LCR], $83			; 8 data, 1 stop, no parity	, divisor latch = 1, UART address 3 = Line Control Register
	mov byte[_UART0_DLAB_0], 3			; baud = 38400, divisor latch low byte = 3
	mov byte[_UART0_DLAB_1], 0			; divisor latch high byte = 0			
	mov byte[_UART0_LCR], 3			; UART address 3 = Line Control Register
	mov byte[_UART0_IER], 1			; enable interrupt: receive data available
	mov byte[_UART0_FCR], 0			; disable FIFO
	sysret
; char in ah
io_putchar:
io_putchar_L0:
	mov al, [_UART0_LSR]			; read Line Status Register
	test al, $20					; isolate Transmitter Empty
	jz io_putchar_L0		
	mov al, ah
	mov [_UART0_DATA], al			; write char to Transmitter Holding Register
	sysret
; char in ah
io_getchar:
	sti
	push b
	push d
io_getchar_L0:	
	sti
	mov a, [fifo_pr]
	mov b, [fifo_pi]
	cmp a, b
	je io_getchar_L0
	
	mov d, a
	mov al, [d]
	push al
	
	mov a, [fifo_pr]
	inc a
	cmp a, fifo + FIFO_SIZE				; check if pointer reached the end of the fifo
	jne io_getchar_cont2
	mov a, fifo	
io_getchar_cont2:	
	mov [fifo_pr], a			; update fifo pointer
	
	pop ah
; here we just echo the char back to the console
io_getchar_echo_L0:
	mov al, [_UART0_LSR]			; read Line Status Register
	test al, $20					; isolate Transmitter Empty
	jz io_getchar_echo_L0
	mov al, ah
	mov [_UART0_DATA], al			; write char to Transmitter Holding Register

	pop d
	pop b
	sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE SYSTEM DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; infor for : IDE SERVICES INTERRUPT
; IDE read/write 512-byte sector
; al = option
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
file_system_jmptbl:
	.dw file_system_mkfs
	.dw cmd_fs_space
	.dw cmd_mkdir
	.dw cmd_cd
	.dw cmd_ls
	.dw cmd_mktxt
	.dw cmd_mkbin
	.dw cmd_pwd
	.dw cmd_cat
	.dw cmd_rmdir
	.dw cmd_rm
	.dw cmd_user_fload
	.dw 0
	.dw 0					; 13
	.dw fileio_chmod		; 14
	.dw cmd_mv				; 15
	.dw cd_goto_root
	.dw cmd_loadfile
	.dw get_path_from_dirID	; 18
	.dw get_dirID_from_path	; 19
	.dw loadfile_from_path	; 20
file_system:
	jmp [file_system_jmptbl + al]

file_system_mkfs:	
	mov di, transient_area
	mov al, 0
	mov c, 512
	rep stosb
	mov b, FST_LBA_START
	mov c, 0				; reset LBA to 0
file_system_mkfs_L1:	
	mov a, $0103			; disk write
	mov d, transient_area
	syscall sys_ide
	inc b
	cmp b, CF_CARD_LBA_SIZE
	jne file_system_mkfs_L1
file_system_mkfs_create_root:
	mov a, ROOT_dirID
	mov [current_dirID], a		; set current directory LBA to ROOT
	sysret	
	
cd_goto_root:
	mov a, ROOT_dirID
	mov [current_dirID], a		; set current directory LBA to ROOT
	sysret	

; filename in D (userspace data)
; permission in BL
fileio_chmod:
	push bl
	mov si, d
	mov di, userspace_data
	mov c, 128
	load					; load filename from user-space
		
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset file counter
fileio_chmod_L1:
	mov si, d
	mov di, userspace_data
	call strcmp
	je fileio_chmod_found_entry

	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	jne fileio_chmod_L1
	pop bl
	jmp fileio_chmod_not_found
fileio_chmod_found_entry:	
	mov g, b					; save LBA
	pop bl						; retrieve saved permission value
	shl bl, 1					; shift left to make space for D flag
	mov al, [d + 24]			; read file permissions
	and al, %00000001			; remove all permissions, keep D flag
	or al, bl					; set new permissions
	mov [d + 24], al			; write new permissions
	mov c, 0
	mov d, transient_area
	mov a, $0103				; disk write 1 sect
	mov b, g					; retrieve LBA
	syscall sys_ide		; write sector
fileio_chmod_not_found:
	sysret
	
cmd_fs_space:
	sysret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE NEW DIRECTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search list for NULL name entry. add new directory to list
cmd_mkdir:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load						; load data from user-space
	mov b, FST_LBA_START + 2 	; start at 2 because LBA  0 is ROOT (this would also cause issues 								
								; when checking for NULL name, since root has a NULL name)
	mov c, 0					; reset LBA to 0
cmd_mkdir_L1:	
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read sector
	mov al, [d]
	cmp al, 0			; check for NULL
	je cmd_mkdir_found_null
	add b, FST_SECTORS_PER_DIR					; skip directory
	jmp cmd_mkdir_L1
cmd_mkdir_found_null:
;create header file by grabbing dir name from parameter
	push b				; save new directory's LBA
	mov c, 64
	mov si, userspace_data
	mov di, transient_area
	rep movsb					; copy dirname from userspace_data to transient_area
	mov a, [current_dirID]
	mov [transient_area + 64], a		; store parent directory LBA
	mov al, 0
	mov di, transient_area + 512
	mov c, 512
	rep stosb					; clean buffer
	mov c, 0				; reset LBA(c) to 0
; write directory entry sectors
	mov d, transient_area
	mov a, $0203			; disk write, 2 sectors
	syscall sys_ide		; write sector
; now we need to add the new directory to the list, inside the current directory
	mov a, [current_dirID]
	add a, 1
	mov b, a					; metadata sector
	mov c, 0
	mov g, b					; save LBA
	mov d, transient_area
	mov a, $0102			; disk read
	syscall sys_ide		; read metadata sector
cmd_mkdir_L2:
	mov al, [d]
	cmp al, 0
	je cmd_mkdir_found_null2
	add d, FST_ENTRY_SIZE
	jmp cmd_mkdir_L2					; we look for a NULL entry here but dont check for limits. CARE NEEDED WHEN ADDING TOO MANY FILES TO A DIRECTORY
cmd_mkdir_found_null2:
	mov si, userspace_data
	mov di, d
	call strcpy			; copy directory name
	add d, 24			; goto ATTRIBUTES
	mov al, %00000111		; no execute, write, read, directory
	mov [d], al			
	inc d
	pop b
	push b				; push LBA back
	mov [d], b			; save LBA
; set file creation date	
	add d, 4
	mov al, 4
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set day
	inc d
	mov al, 5
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set month
	inc d
	mov al, 6
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set year
; write sector into disk for new directory entry
	mov b, g
	mov c, 0
	mov d, transient_area
	mov a, $0103			; disk write, 1 sector
	syscall sys_ide		; write sector

; after adding the new directory's information to its parent directory's list
; we need to now enter the new directory, and to it add two new directories!
; which directories do we need to add ? '..' and '.' are the directories needed.
; importantly, note that these two new directories are only entries in the list
; and do not have actual physical entries in the disk as real directories.
; i.e. they only exist as list entries in the new directory created so that
; the new directory can reference its parent and itself
;
; we need to add both '..' and '.'
; this first section is for '..' and on the section below we do the same for '.'
	pop a						; retrieve the new directory's LBA	
	push a						; and save again
	add a, 1
	mov b, a					; metadata sector
	mov c, 0
	mov g, b					; save LBA
	mov d, transient_area
	mov a, $0102			; disk read
	syscall sys_ide		; read metadata sector
cmd_mkdir_L3:
	mov al, [d]
	cmp al, 0
	je cmd_mkdir_found_null3
	add d, FST_ENTRY_SIZE
	jmp cmd_mkdir_L3	; we look for a NULL entry here but dont check for limits. CARE NEEDED WHEN ADDING TOO MANY FILES TO A DIRECTORY
cmd_mkdir_found_null3:
	mov si, s_parent_dir
	mov di, d
	call strcpy			; copy directory name
	add d, 24			; goto ATTRIBUTES
	mov al, %00000111		; no execute, write, read, directory
	mov [d], al			
	inc d
	mov b, [current_dirID]	; retrieve the parent directorys LBA
	mov [d], b			; save LBA
; set file creation date	
	add d, 4
	mov al, 4
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set day
	inc d
	mov al, 5
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set month
	inc d
	mov al, 6
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set year
; write sector into disk for new directory entry
	mov b, g
	mov c, 0
	mov d, transient_area
	mov a, $0103			; disk write, 1 sector
	syscall sys_ide		; write sector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; like we did above for '..', we need to now add the '.' directory to the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	pop a						; retrieve the new directory's LBA	
	push a
	add a, 1
	mov b, a					; metadata sector
	mov c, 0
	mov g, b					; save LBA
	mov d, transient_area
	mov a, $0102				; disk read
	syscall sys_ide				; read metadata sector
cmd_mkdir_L4:
	mov al, [d]
	cmp al, 0
	je cmd_mkdir_found_null4
	add d, FST_ENTRY_SIZE
	jmp cmd_mkdir_L4	; we look for a NULL entry here but dont check for limits. CARE NEEDED WHEN ADDING TOO MANY FILES TO A DIRECTORY
cmd_mkdir_found_null4:
	mov si, s_current_dir
	mov di, d
	call strcpy			; copy directory name
	add d, 24			; goto ATTRIBUTES
	mov al, %00000111		; no execute, write, read, directory
	mov [d], al			
	inc d
	pop b				; new directory's LBA itself. for self-referential directory entry '.'
	mov [d], b			; save LBA
; set file creation date	
	add d, 4
	mov al, 4
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set day
	inc d
	mov al, 5
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set month
	inc d
	mov al, 6
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set year
; write sector into disk for new directory entry
	mov b, g
	mov c, 0
	mov d, transient_area
	mov a, $0103			; disk write, 1 sector
	syscall sys_ide		; write sector
cmd_mkdir_end:
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get path from a given directory dirID
;; pseudo code:
;	get_path_from_dirID(int dirID, char *D){
;		if(dirID == 0){
;			reverse path in D;
;			return;
;		}
;		else{
;			copy directory name to end of D;
;			add '/' to end of D;
;			parentID = get parent directory ID;
;			get_path_from_dirID(parentID, D);
;		}
;	}
;; A = dirID
;; D = generated path string pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample path: /usr/bin
get_path_from_dirID:
	mov d, filename
	mov al, 0
	mov [d], al				; initialize path string 
	mov a, [current_dirID]
	call get_path_from_dirID_E0
	mov d, filename
	call strrev
	call puts
	sysret
get_path_from_dirID_E0:
	call get_dirname_from_dirID
	mov si, s_slash
	mov di, d
	call strcat						; add '/' to end of path
	cmp a, ROOT_dirID				; check if we are at the root directory
	je get_path_from_dirID_root
	call get_parentID_from_dirID	; use current ID (A) to find parentID (into A)
	cmp a, ROOT_dirID				; check if we are at the root directory
	je get_path_from_dirID_root
	call get_path_from_dirID_E0		; recursively call itself
get_path_from_dirID_root:
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inputs:
;; A = directory ID
;; outputs:
;; D = pointer to directory name string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_dirname_from_dirID:
	push a
	push b
	push d
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area - 512
	syscall sys_ide			; read directory
	call strrev				; reverse dir name before copying
	mov si, d
	pop d					; destination address = D value pushed at beginning
	mov di, d
	call strcat				; copy filename to D
	pop b
	pop a
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inputs:
;; A = directory ID
;; outputs:
;; A = parent directory ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_parentID_from_dirID:
	push b
	push d
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area - 512
	syscall sys_ide			; read directory
	mov a, [d + 64]			; copy parent ID value to A
	pop d
	pop b
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get dirID from a given path string
;; inputs:
;; D = path pointer (path inside kernel space, not userspace!)
;; outputs:
;; A = dirID
;; /usr/local/bin		- absolute
;; local/bin/games		- relative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_dirID_from_path:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load
	mov b, userspace_data
	mov [prog], b			; token pointer set to path string
	call get_token
	mov bl, [tok]
	cmp bl, TOK_FSLASH
	je get_dirID_from_path_abs 
	mov a, [current_dirID]
	call putback
	jmp get_dirID_from_path_E0
get_dirID_from_path_abs:
	mov a, ROOT_dirID
get_dirID_from_path_E0:
	call get_token
	mov bl, [toktyp]
	cmp bl, TOKTYP_IDENTIFIER
	jne get_dirID_from_path_end	; check if there are tokens after '/'. i.e. is this a 'cd /' command?

	mov si, tokstr
	mov di, filename
	call strcpy				
	inc a					; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a
get_dirID_from_path_L1:
	mov al, [d + 24]
	and al, %00000011			; isolate read and directory flags
	cmp al, %00000011
	jne get_dirID_from_path_no_perm
	mov si, d
	mov di, filename
	call strcmp
	je get_dirID_from_path_name_equal	
get_dirID_from_path_no_perm:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je get_dirID_from_path_end
	jmp get_dirID_from_path_L1
get_dirID_from_path_name_equal:
	add d, 25					; 
	mov a, [d]					; set result register A = dirID
	call get_token
	mov bl, [tok]
	cmp bl, TOK_FSLASH			; check if there are more elements in the path
	je get_dirID_from_path_E0
	call putback
get_dirID_from_path_end:
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load file data from a given path string
;; inputs:
;; D = path pointer (path inside kernel space, not userspace!)
;; DI = userspace program data destination
;; A = error code
;; /usr/local/bin/ed
;; ./ed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
loadfile_from_path:
	push di
	mov si, d
	mov di, userspace_data
	mov c, 256
	load
	mov b, userspace_data
	mov [prog], b			; token pointer set to path string
	call get_token
	mov bl, [tok]
	cmp bl, TOK_FSLASH
	je loadfile_from_path_abs 
	mov a, [current_dirID]
	call putback
	jmp loadfile_from_path_E0
loadfile_from_path_abs:
	mov a, ROOT_dirID
loadfile_from_path_E0:
	call get_token
	mov bl, [toktyp]
	cmp bl, TOKTYP_IDENTIFIER
	jne loadfile_from_path_end	; check if there are tokens after '/'. i.e. is this a 'cd /' command?

	mov si, tokstr
	mov di, filename
	call strcpy				
	inc a					; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a
loadfile_from_path_L1:
	mov si, d
	mov di, filename
	call strcmp
	je loadfile_from_path_name_equal	
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je loadfile_from_path_end
	jmp loadfile_from_path_L1
loadfile_from_path_name_equal:
	mov bl, [d + 24]
	and bl, %00000001			; directory flag
	jnz loadfile_isdirectory	;
; entry is a file
	add d, 25			; get to dirID of file in disk
	mov b, [d]			; get LBA
	inc b				; add 1 to B because the LBA for data comes after the header sector
	mov d, transient_area
	mov c, 0
	mov ah, FS_SECTORS_PER_FILE-1		; number of sectors
	mov al, $02							; disk read
	syscall sys_ide				; read sector
	pop di
	mov si, transient_area
	mov c, 512*(FS_SECTORS_PER_FILE-1)
	store
	mov a, 1			; success code
	sysret
loadfile_isdirectory:
	add d, 25					; 
	mov a, [d]					; set result register A = dirID
	call get_token
	jmp loadfile_from_path_E0
loadfile_from_path_end:
	mov a, 0
	pop di
	sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search for given directory inside current dir
; if found, read its LBA, and switch directories
; new dirID in B
cmd_cd:
	mov [current_dirID], b
	sysret	

	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; load data from user-space
	mov a, [current_dirID]
	inc a					; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a
cmd_cd_L1:
	mov al, [d + 24]
	and al, %00000011			; isolate read and directory flags
	cmp al, %00000011
	jne cmd_cd_no_permission
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_cd_name_equal	
cmd_cd_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_cd_end
	jmp cmd_cd_L1
cmd_cd_name_equal:
	add d, 25					; set new dirID
	mov a, [d]
	mov [current_dirID], a	
cmd_cd_end:
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_ls:	
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset entry index
cmd_ls_L1:
	mov al, [d]
	cmp al, 0			; check for NULL
	je cmd_ls_next
cmd_ls_non_null:
	mov al, [d + 24]
	and al, %00000001
	mov ah, 0
	mov a, [a + file_attrib]		; directory?
	mov ah, al
	call putchar
	mov al, [d + 24]
	and al, %00000010
	mov ah, 0
	mov a, [a + file_attrib]		; read
	mov ah, al
	call putchar
	mov al, [d + 24]
	and al, %00000100
	mov ah, 0
	mov a, [a + file_attrib]		; write
	mov ah, al
	call putchar
	mov al, [d + 24]
	and al, %00001000
	mov ah, 0
	mov a, [a + file_attrib]		; execute
	mov ah, al
	call putchar
	mov ah, $20
	call putchar	
	mov a, [d + 27]
	call print_u16d				; filesize
	mov ah, $20
	call putchar	
	mov a, [d + 25]
	call print_u16d			; dirID / LBA
	mov ah, $20
	call putchar
; print date
	mov bl, [d + 29]			; day
	call print_u8x
	mov ah, $20
	call putchar	
	mov al, [d + 30]			; month
	shl al, 2
	push d
	mov d, s_months
	mov ah, 0
	add d, a
	call puts
	pop d
	mov ah, $20
	call putchar
	mov bl, $20
	call print_u8x
	mov bl, [d + 31]			; year
	call print_u8x	
	mov ah, $20
	call putchar	
	call puts				; print filename	
	call printnl
cmd_ls_next:
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_ls_end
	add d, 32			
	jmp cmd_ls_L1	
cmd_ls_end:
	sysret

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finds an empty data block
;; block LBA returned in B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fs_find_empty_block:
	mov b, FS_LBA_START		; raw files starting block
	mov c, 0						; reset LBA to 0
fs_find_empty_block_L1:	
	mov a, $0102			; disk read
	mov d, transient_area - 512
	syscall sys_ide		; read sector
	mov al, [d]
	cmp al, 0			; check for NULL
	je fs_find_empty_block_found_null
	add b, FS_SECTORS_PER_FILE
	jmp fs_find_empty_block_L1
fs_find_empty_block_found_null:
	ret

; file structure:
; 512 bytes header
; header used to tell whether the block is free
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE NEW TEXTFILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_mktxt:
	mov si, d
	mov di, transient_area
	mov c, FS_SECTORS_PER_FILE * 512
	load					; load data from user-space
	call fs_find_empty_block	; look for empty data blocks
	push b				; save empty block LBA
;create header file by grabbing file name from parameter	
	mov d, transient_area + 512			; pointer to file contents
	;call gettxt
	call strlen						; get length of the text file
	push c							; save length
	mov al, 1
	mov [transient_area], al					; mark sectors as USED (not NULL)
	mov d, transient_area
cmd_mktxt_L2:
	mov c, 0
	mov ah, FS_SECTORS_PER_FILE		; number of sectors to write
	mov al, $03						; disk write
	syscall sys_ide			; write sectors
; now we add the file to the current directory!
cmd_mktxt_add_to_dir:	
	mov a, [current_dirID]
	inc a
	mov b, a					; metadata sector
	mov c, 0
	mov g, b					; save LBA
	mov d, transient_area - 512
	mov a, $0102			; disk read
	syscall sys_ide		; read metadata sector
cmd_mktxt_add_to_dir_L2:
	mov al, [d]
	cmp al, 0
	je cmd_mktxt_add_to_dir_null
	add d, FST_ENTRY_SIZE
	jmp cmd_mktxt_add_to_dir_L2		; we look for a NULL entry here but dont check for limits. CARE NEEDED WHEN ADDING TOO MANY FILES TO A DIRECTORY
cmd_mktxt_add_to_dir_null:
	mov si, transient_area + 1		; filename located after the data block 'USED' marker byte
	mov di, d
	call strcpy			; copy file name
	add d, 24			; skip name
	mov al, %00000110		; no execute, write, read, not directory
	mov [d], al			
	add d, 3
	pop a
	mov [d], a
	sub d, 2
	pop b				; get file LBA
	mov [d], b			; save LBA	
	
	; set file creation date	
	add d, 4
	mov al, 4
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set day
	
	inc d
	mov al, 5
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set month
	
	inc d
	mov al, 6
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set year
	
; write sector into disk for new directory entry
	mov b, g
	mov c, 0
	mov d, transient_area - 512
	mov a, $0103			; disk write, 1 sector
	syscall sys_ide		; write sector
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE NEW BINARY FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; search for first null block
cmd_mkbin:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; load data from user-space
	
	mov b, FS_LBA_START		; files start when directories end
	mov c, 0						; reset LBA to 0
cmd_mkbin_L1:	
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read sector
	mov al, [d]
	cmp al, 0			; check for NULL
	je cmd_mkbin_found_null
	add b, FS_SECTORS_PER_FILE
	jmp cmd_mkbin_L1
cmd_mkbin_found_null:
	push b				; save LBA
;create header file by grabbing file name from parameter
	mov di, transient_area + 512	; pointer to file contents
	call _load_hex			; load binary hex
	push c					; save size (nbr of bytes)
	mov al, 1
	mov [transient_area], al		; mark sectors as USED (not NULL)
	mov a, 0
	mov [index], a
	mov d, transient_area
	mov a, d
	mov [buffer_addr], a
cmd_mkbin_L2:
	mov c, 0
	mov a, $0103				; disk write, 1 sector
	syscall sys_ide			; write sector
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FS_SECTORS_PER_FILE		; remove 1 from this because we dont count the header sector
	je cmd_mkbin_add_to_dir
	inc b
	mov a, [buffer_addr]
	add a, 512
	mov [buffer_addr], a
	mov d, a
	jmp cmd_mkbin_L2
; now we add the file to the current directory!
cmd_mkbin_add_to_dir:	
	mov a, [current_dirID]
	inc a
	mov b, a					; metadata sector
	mov c, 0
	mov g, b					; save LBA
	mov d, transient_area
	mov a, $0102			; disk read
	syscall sys_ide		; read metadata sector
cmd_mkbin_add_to_dir_L2:
	mov al, [d]
	cmp al, 0
	je cmd_mkbin_add_to_dir_null
	add d, FST_ENTRY_SIZE
	jmp cmd_mkbin_add_to_dir_L2					; we look for a NULL entry here but dont check for limits. CARE NEEDED WHEN ADDING TOO MANY FILES TO A DIRECTORY
cmd_mkbin_add_to_dir_null:
	mov si, userspace_data
	mov di, d
	call strcpy			; copy file name
	add d, 24			; skip name
	mov al, %00001110		; execute, write, read, not directory
	mov [d], al
	add d, 3
	pop a
	mov [d], a
	sub d, 2
	pop b				; get file LBA
	mov [d], b			; save LBA
	
	; set file creation date	
	add d, 4
	mov al, 4
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set day
	
	inc d
	mov al, 5
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set month
	
	inc d
	mov al, 6
	syscall sys_rtc
	mov al, ah
	mov [d], al			; set year
	
; write sector into disk for new directory entry
	mov b, g
	mov c, 0
	mov d, transient_area
	mov a, $0103			; disk write, 1 sector
	syscall sys_ide		; write sector
	sysret

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PWD - PRINT WORKING DIRECTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
cmd_pwd:
	mov d, filename
	mov al, 0
	mov [d], al				; initialize path string 
	mov a, [current_dirID]
	call get_path_from_dirID_E0
	mov d, filename
	call strrev
	call puts
	call printnl
	sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get current directory LBA
;; A: returned LBA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
cmd_get_curr_dir_LBA:
	mov a, [current_dirID]
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD FILE INTO MEM
;; file loaded to transient_area
;; D: filename pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
cmd_loadfile:
	mov si, d
	mov di, filename
	call strcpy
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area-512
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset file counter
cmd_loadfile_L1:
	mov al, [d + 24]		; get to file attributes (needs to be executable and not a directory)
	and al, %00001001		; isolate executable and directory flags
	cmp al, %00001000
	jne cmd_loadfile_no_permission
	mov si, d
	mov di, filename
	call strcmp
	je cmd_loadfile_found_entry
cmd_loadfile_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_loadfile_not_found
	jmp cmd_loadfile_L1
cmd_loadfile_found_entry:
	add d, 25			; get to dirID of file in disk
	mov b, [d]			; get LBA
	inc b				; add 1 to B because the LBA for data comes after the header sector
	mov d, transient_area
	mov ah, FS_SECTORS_PER_FILE-1		; number of sectors
	mov al, $02							; disk read
	syscall sys_ide				; read sector
cmd_loadfile_not_found:
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAT
;; userspace destination data pointer in D
;; filename starts at D, but is overwritten after the read is made
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
cmd_cat:
	push d					; save userspace file data destination
	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; copy filename from user-space
	mov b, [current_dirID]
	inc b				; metadata sector
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area-512
	syscall sys_ide		; read directory
	cla
	mov [index], a		; reset file counter
cmd_cat_L1:
	mov al, [d + 24]		; isolate directory and read flags
	and al, %00001011
	cmp al, %00000010
	jne cmd_cat_no_permission
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_cat_found_entry
cmd_cat_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_cat_not_found
	jmp cmd_cat_L1
cmd_cat_found_entry:
	add d, 25			; get to dirID of file in disk
	mov b, [d]			; get LBA
	inc b				; add 1 to B because the LBA for data comes after the header sector 
	mov d, transient_area	
	mov c, 0
	mov ah, FS_SECTORS_PER_FILE-1	; nbr sectors
	mov al, $02						; disk read 
	syscall sys_ide					; read sectors
	pop di						; write userspace file data destination to DI
	mov si, transient_area		; data origin
	mov c, 512*(FS_SECTORS_PER_FILE-1)
	store
	sysret
cmd_cat_not_found:
	pop d
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RMDIR - remove DIR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deletes directory  entry in the current directory's file list 
; also deletes the actual directory entry in the FST
cmd_rmdir:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; load data from user-space
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset file counter
cmd_rmdir_L1:
	mov al, [d + 24]		; get to file type (needs to be a directory and writeable)
	and al, %00000101	
	cmp al, %00000101
	jne cmd_rmdir_no_permission
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_rmdir_found_entry
cmd_rmdir_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_rmdir_not_found
	jmp cmd_rmdir_L1
cmd_rmdir_found_entry:
	mov b, [d + 25]			; get LBA
	mov g, b				; save LBA
	mov al, 0
	mov [d], al			; make file entry NULL
	
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0103			; disk write
	mov d, transient_area
	syscall sys_ide		; write sector and erase file's entry in the current DIR
		
	mov d, transient_area	
	mov al, 0
	mov [d], al			; make file's data header NULL for re-use

	mov c, 0
	mov b, g				; get data header LBA
	mov a, $0103					; disk write 1 sect
	syscall sys_ide				; write sector
cmd_rmdir_not_found:	
	sysret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RM - remove file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; frees up the data sectors for the file further down the disk
; deletes file entry in the current directory's file list 
cmd_rm:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; load data from user-space
	
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset file counter
cmd_rm_L1:
	mov al, [d + 24]		; get to file type
	and al, %00000101		; isolate write and directory flags
	cmp al, %00000100
	jne cmd_rm_no_permission
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_rm_found_entry
cmd_rm_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_rm_not_found
	jmp cmd_rm_L1
cmd_rm_found_entry:
	mov b, [d + 25]			; get LBA
	mov g, b				; save LBA
	mov al, 0
	mov [d], al			; make file entry NULL
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0103			; disk write
	mov d, transient_area
	syscall sys_ide		; write sector and erase file's entry in the current DIR
	mov d, transient_area	
	mov al, 0
	mov [d], al			; make file's data header NULL for re-use
	mov c, 0
	mov b, g				; get data header LBA
	mov a, $0103					; disk write 1 sect
	syscall sys_ide				; write sector
cmd_rm_not_found:	
	sysret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mv - move / change file name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_mv:
	mov si, d
	mov di, userspace_data
	mov c, 256
	load						; load data from user-space
	
	mov a, [current_dirID]
	inc a						; metadata sector
	mov b, a	
	mov c, 0					; reset LBA to 0
	mov a, $0102				; disk read
	mov d, transient_area
	syscall sys_ide		;read directory
	mov a, 0
	mov [index], a				;reset file counter
cmd_mv_L1:
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_mv_found_entry
cmd_mv_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_mv_not_found
	jmp cmd_mv_L1
cmd_mv_found_entry:	
	push bl
	mov si, userspace_data+128	; (0...127) = original filename , (128...255) = new name
	mov di, d
	call strcpy	
	mov c, 0
	mov d, transient_area
	mov a, $0103					;disk write 1 sect
	pop bl
	syscall sys_ide			;write sector
cmd_mv_not_found:
	sysret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPEN PROGRAM/FILE
;; userspace program destination address in B
;; filename in d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd_user_fload:
	push b			; save user destination memory for program
	mov si, d
	mov di, userspace_data
	mov c, 256
	load					; load data from user-space
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a		; reset file counter
cmd_user_fload_L1:
	mov al, [d + 24]		; get to file attributes (needs to be executable and not a directory)
	and al, %00001001		; isolate executable and directory flags
	cmp al, %00001000
	jne cmd_user_fload_no_permission
	mov si, d
	mov di, userspace_data
	call strcmp
	je cmd_user_fload_found_entry
cmd_user_fload_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je cmd_user_fload_not_found
	jmp cmd_user_fload_L1
cmd_user_fload_found_entry:
	add d, 25			; get to dirID of file in disk
	mov b, [d]			; get LBA
	inc b				; add 1 to B because the LBA for data comes after the header sector
	mov d, transient_area
	mov ah, FS_SECTORS_PER_FILE-1		; number of sectors
	mov al, $02							; disk read
	syscall sys_ide				; read sector
cmd_user_fload_found_end:				
	pop di								; retrieve userspace program destination address
	mov si, transient_area
	mov c, 512*(FS_SECTORS_PER_FILE-1)		; now copy all program sectors into user space
	store
cmd_user_fload_not_found:
	sysret

KERNEL_RESET_VECTOR:	
	mov bp, STACK_BEGIN
	mov sp, STACK_BEGIN
	
	mov al, %10000000
	stmsk					; mask out timer interrupt for now (only allow UART to interrupt)
	sti	
	
; reset fifo pointers
	mov a, fifo
	mov d, fifo_pi
	mov [d], a
	mov d, fifo_pr
	mov [d], a	
	mov al, 2
	syscall sys_io			; enable uart in interrupt mode
	
	mov d, s_started
	call puts
	
; here we need to launch the shell
; now we allocate a new process	
	call find_free_proc			; index in A
	setptb
	call proc_memory_map			; map process memory pages
			
	mov si, s_shell_bin
	mov di, proc_names + 32
	call strcpy				; copy shell process name
		
	mov d, s_shell_1
	call puts
	call load_shell
	
	mov al, 16
	syscall sys_fileio		; set root dirID

; now copy process binary data into process1 memory
	mov si, transient_area
	mov di, 0
	mov c, 10000				; mov c is not needed because _load_hex returns the size of the program in C
	store										; copy process data
		
	call find_free_proc			; index in A
	mov [active_proc_index], al		; set new active process
	mov d, a
	mov al, 1
	mov [d + proc_availability_table], al		; make process busy
	
	mov al, [nbr_active_procs]					; increase nbr of active processes
	inc al
	mov [nbr_active_procs], al
							; here we copy the value of the RESET VECTOR of the new process
	mov a, [transient_area]	; so that we can jump to it upon running the process
							; the first word address is the RESET VECTOR
; launch process
	push word $FFFF 
	push byte %00000100
	push a				; and then push RESET VECTOR of the shell to the stack
	sysret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Index in A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
find_free_proc:
	mov si, proc_availability_table + 1			; skip process 0 (kernel)
find_free_proc_L0:
	lodsb						; get process state
	cmp al, 0
	je find_free_proc_free			; if free, jump
	jmp find_free_proc_L0			; else, not busy, goto next
find_free_proc_free:
	mov a, si
	sub a, 1 + proc_availability_table				; get process index
	ret
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Index in AL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc_memory_map:
	mov ah, 0
	mov b, a			; page in BL, 0 in BH
	shl a, 5			; multiply by 32
	mov c, a			; save in C
	add c, 32
proc_memory_map_L0:
	pagemap
	add b, $0800					; increase page number (msb 5 bits of BH only)
	add a, 1						; increase both 
	cmp a, c						; check to see if we reached the end of memory
	jne proc_memory_map_L0
	ret
	
cmd_fork:
	mov al, %10000000				; deactivate timer interrupt
	stmsk
; we save the active process first	
	pusha
	mov ah, 0
	mov al, [active_proc_index]
	shl a							; x2
	mov a, [PROC_TABLE_convert + a]		; get process state start index
		
	mov di, a
	mov a, sp
	inc a
	mov si, a
	mov c, 20
	rep movsb					; save process state!
; restore kernel stack position to point before interrupt arrived
	add sp, 20
	
;; now we allocate a new process	
	call find_free_proc			; index in A
	setptb
	call proc_memory_map			; map process memory pages
			
	mov di, transient_area				
	call _load_hex
; now copy process binary data into process1 memory
	mov si, transient_area
	mov di, 0
	mov c, 10000					; mov c is not needed because _load_hex returns the size of the program in C
	store						; copy process data
		
	call find_free_proc			; index in A
	mov [active_proc_index], al		; set new active process
	shl a, 5						; x32
	call printnl
	mov d, s_procname
	call puts
	mov d, a
	add d, proc_names
	call gets					; get proc name
	call printnl
	
	call find_free_proc			; index in A
	mov d, a
	mov al, 1
	mov [d + proc_availability_table], al					; make process busy
	
	mov al, [nbr_active_procs]			; increase nbr of active processes
	inc al
	mov [nbr_active_procs], al
	
	mov al, %10000000
	stmsk					; mask out timer interrupt for now (only allow UART to interrupt)
	
; launch process
	push word $FFFF 
	push byte %00000100
	push word 0
	sysret
	
PROC_TABLE_convert:
	.dw proc_state_table + 0
	.dw proc_state_table + 20
	.dw proc_state_table + 40
	.dw proc_state_table + 60
	.dw proc_state_table + 80
	.dw proc_state_table + 100
	.dw proc_state_table + 120
	.dw proc_state_table + 140
	

load_shell:
	mov al, 16
	syscall sys_fileio				; cd to /
; cd to /bin
	mov a, [current_dirID]
	inc a				; metadata sector
	mov b, a
	mov c, 0				; reset LBA to 0
	mov a, $0102			; disk read
	mov d, transient_area-512
	syscall sys_ide		; read directory
	mov a, 0
	mov [index], a
load_shell_L1:
	mov al, [d + 24]
	and al, %00000011			; isolate read and directory flags
	cmp al, %00000011
	jne load_shell_no_permission
	mov si, d
	mov di, s_sbin_dir
	call strcmp
	je load_shell_name_equal	
load_shell_no_permission:
	add d, 32
	mov a, [index]
	inc a
	mov [index], a
	cmp a, FST_FILES_PER_DIR
	je load_shell_end
	jmp load_shell_L1
load_shell_name_equal:
	add d, 25
	mov a, [d]
	mov [current_dirID], a	
load_shell_end:
	mov d, s_shell_bin				; shell filename pointer
	mov al, 17
	syscall sys_fileio				; load shell.bin binary data to transient_area
	ret


cmd_fwb:
	mov di, transient_area	; pointer to file contents
	call _load_hex			; load binary hex
	mov b, 0
	mov c, 0
	mov a, $0103				; disk write, 1 sector
	mov d, transient_area
	syscall sys_ide			; write sector
	call printnl
	sysret
	
cmd_fwk:
	mov di, transient_area	; pointer to file contents
	call _load_hex			; load binary hex
	mov a, 0
	mov [index], a
	mov d, transient_area
	mov a, d
	mov [buffer_addr], a
	mov b, 1
	mov c, 0
cmd_fwrite_L1:	
	mov a, $0103				; disk write, 1 sector
	syscall sys_ide			; write sector
	mov a, [index]
	inc a
	mov [index], a
	cmp a, 31		
	je cmd_fwrite_end
	inc b
	mov a, [buffer_addr]
	add a, 512
	mov [buffer_addr], a
	mov d, a
	jmp cmd_fwrite_L1
cmd_fwrite_end:
	call printnl
	sysret

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

; synopsis: look inside a certain DIRECTORY for files/directories
; BEFORE CALLING THIS FUNCTION, CD INTO REQUIRED DIRECTORY
; for each entry inside DIRECTORY:
;	if entry is a file:
;		compare filename to searched filename
;		if filenames are the same, print filename
;	else if entry is a directory:
;		cd to the given directory
;		recursively call cmd_find
;		cd outside previous directory
;	if current entry == last entry, return
; endfor
;		
;	
f_find:
	
	ret



nbr_active_procs:			.db 0
active_proc_index:			.db 1
proc_state_table:			.fill 16 * 20, 0		; for 15 processes max
proc_availability_table:	.fill 16, 0			; space for 15 processes. 0 = process empty, 1 = process busy	
proc_names:					.fill 16 * 32, 0			; process names

index:				.dw 0
buffer_addr:		.dw 0

fifo_pi:			.dw fifo
fifo_pr:			.dw fifo
fifo:				.fill FIFO_SIZE

;  user space data
userspace_data:		.fill 256, 0

; file system variables
current_dirID:		.dw 0				; keep dirID of current directory
root_dir_str:		.db "/", 0
filename:			.fill 512, 0		; holds a path for file search

s_shell_bin:		.db "shell.bin", 0
s_sbin_dir:			.db "sbin", 0

file_attrib:		.db "-dr w   x"			; chars at multiples of 2

s_host: 			.db "Sol-1:", 0

s_int_en:			.db "interrupts enabled\n\r", 0
s_started:			.db "kernel started\n\r", 0
s_procname:			.db "enter process name: ", 0
s_divzero:			.db "\nexception: zero division\n\r", 0
s_shell_1:			.db "launching shell process...\n", 0
s_rebooting: 		.db "rebooting...\n", 0

s_priviledge:		.db "\n\rsoftware failure: privilege exception\n\r", 0

s_set_year:			.db "Year: ", 0
s_set_month:		.db "Month: ", 0
s_set_day:			.db "Day: ", 0
s_set_week:			.db "Weekday: ", 0
s_set_hours:		.db "Hours: ", 0
s_set_minutes:		.db "Minutes: ", 0
s_set_seconds:		.db "Seconds: ", 0

s_months:			.db "   ", 0
					.db "Jan", 0
					.db "Feb", 0
					.db "Mar", 0
					.db "Apr", 0
					.db "May", 0
					.db "Jun", 0
					.db "Jul", 0
					.db "Aug", 0
					.db "Sep", 0
					.db "Oct", 0
					.db "Nov", 0
					.db "Dec", 0

s_week:				.db "Sun", 0 
					.db "Mon", 0 
					.db "Tue", 0 
					.db "Wed", 0 
					.db "Thu", 0 
					.db "Fri", 0 
					.db "Sat", 0

s_parent_dir:		.db "..", 0
s_current_dir:		.db ".", 0
s_slash: 			.db "/", 0
s_hash:				.db " # ", 0
	
					.fill 512
transient_area:		.db 0			; beginning of the transient memory area. used for disk reads and other purposes		



.end
