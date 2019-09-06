[BITS 16]
[ORG 0]

jmp start
	
start:  
    mov ax, 0x7c0
    mov ds, ax
    mov es, ax
    mov ax, 0x900
    mov ss, ax
    mov ax, 0x0
    mov sp, ax
    
    push dword 0x7c0
    push after_update_cs
    retf

after_update_cs:
    sti ; enable irq

    push es
   
    ; set new int9 entry
    mov si, int9_new

    mov ax, 0x0
    mov es, ax
    mov di, 0x9*0x4
    mov cx, 0x2
    cld
    rep movsw
    pop es

    jmp  run

run:
    ; prepare taskB
    mov ss, [taskB_context +  0]
    mov ds, [taskB_context +  4]
    mov sp, [taskB_context + 12]
  
    push word [taskB_context + 26]  ;flag
    push word [taskB_context + 24]  ;cs
    push word [taskB_context + 22]  ;ip
    pusha
    push ds
    push es
    push ss
    mov [tasks+4], ss
    mov [tasks+6], sp
    

    ; prepare taskC
    mov ss, [taskC_context +  0]
    mov ds, [taskC_context +  4]
    mov sp, [taskC_context + 12]
  
    push word [taskC_context + 26]  ;flag
    push word [taskC_context + 24]  ;cs
    push word [taskC_context + 22]  ;ip
    pusha
    push ds
    push es
    push ss
    mov [tasks+8], ss
    mov [tasks+10], sp

    ; prepare taskA and run it
    mov ss, [taskA_context +  0]
    mov ds, [taskA_context +  4]
    mov sp, [taskA_context + 12]
  
    mov [tasks+0], ss
    mov [tasks+2], sp

    push word [taskA_context + 26]  ;flag
    push word [taskA_context + 24]  ;cs
    push word [taskA_context + 22]  ;ip

    iret    ; kick the taskA

taskA:
    mov si, msgA
    call write_message
    call delay
    jmp taskA

taskB:
    mov si, msgB
    call write_message
    call delay
    jmp taskB

taskC:
    mov si, msgC
    call write_message
    call delay
    jmp taskC

delay:
    push ax
    mov ax, 0xFFFF
dloop:
    sub ax, 1
    cmp ax, 0
    je  end_dloop
    pusha
    popa
    jmp dloop
end_dloop:
    pop ax
    ret

int9_entry:
    ; cpu context save
    pusha
    push ds
    push es
    push ss

    ; read scan code
    ; break code = make code | 0x80
    in al, 0x60
    and ax, 0x0080
    jz  kmake
    jmp kbreak

kmake:
    xor cx, cx
    mov cx, word [index]
    xor di, di
    mov di, cx
    cmp cx, 2
    jz hi
    inc cx
    jmp hello
hi:
    xor cx, cx
hello:
    mov word [index], cx
    mov bx, cx
        
    shl bx, 2
    
    shl di, 2

    mov [tasks+di], ss
    mov [tasks+di+2], sp

    mov ax, [tasks+bx]
    mov cx, [tasks+bx+2]

    mov ss, ax
    mov sp, cx
    
    jmp end

kbreak:
end:
    ;end of irq
    mov al, 0x20
    mov dx, 0x20
    out dx, al

    ; cpu context restore
    pop ss
    pop es
    pop ds
    popa
    iret
	
write_message:
    pusha   ;ax bx cx dx sp bp si di
again:
    lodsb			; DS:[SI] is read to al
	cmp	al, 0x0
	jz	end_message
	mov	ah, 0x0E	; teletype Mode
	mov	bx, 0007	; white on black attribute
	int	0x10
	jmp	again
	
end_message:
    popa
    ret

msgA	    db	'task A', 0xD, 0xA, 0
msgB	    db	'task B', 0xD, 0xA, 0
msgC	    db	'task C', 0xD, 0xA, 0

int9_new:
    dw  int9_entry  ; ip
    dw  0x7c0       ; cs

tasks:
    dw 0x0  ;  ss
    dw 0x0  ;  sp
    
    dw 0x0  ;  ss
    dw 0x0  ;  sp

    dw 0x0  ;  ss
    dw 0x0  ;  sp

index dw	0x0


taskA_context:
    dw  0x1000  ; ss   0x10000 - 0x11000
    dw  0       ; es
    dw  0x7c0   ; ds
    dw  0       ; di
    dw  0       ; si
    dw  0       ; bp
    dw  0xfffc  ; sp
    dw  0       ; bx
    dw  0       ; dx
    dw  0       ; cx
    dw  0       ; ax

    dw  taskA   ; ip
    dw  0x7c0   ; cs
    dw  0x200   ; flag, enable irq

taskB_context:
    dw  0x1100  ; ss   0x11000 - 0x12000
    dw  0       ; es
    dw  0x7c0   ; ds
    dw  0       ; di
    dw  0       ; si
    dw  0       ; bp
    dw  0xfffc  ; sp
    dw  0       ; bx
    dw  0       ; dx
    dw  0       ; cx
    dw  0       ; ax

    dw  taskB   ; ip
    dw  0x7c0   ; cs
    dw  0x200   ; flag, enable irq

taskC_context:
    dw  0x1200  ; ss   0x11000 - 0x12000
    dw  0       ; es
    dw  0x7c0   ; ds
    dw  0       ; di
    dw  0       ; si
    dw  0       ; bp
    dw  0xfffc  ; sp
    dw  0       ; bx
    dw  0       ; dx
    dw  0       ; cx
    dw  0       ; ax

    dw  taskC   ; ip
    dw  0x7c0   ; cs
    dw  0x200   ; flag, enable irq
times 510-($-$$) db 0
dw 0xAA55
