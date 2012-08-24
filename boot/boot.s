;
;	boot.s
;
; boot.s is loaded at 0x7c00 by the bios-startup routines, and moves itself
; out of the way to address 0x90000, and jumps there.
;
; boot.s ��������ɵĴ���ᱻ BIOS ���ص� 0x7C00 �ĵ�ַ����
; Ȼ��ᱻ�����ƶ��� 0x90000 ��ִ�С�
;
; It then loads the system at 0x10000, using BIOS interrupts. Thereafter
; it disables all interrupts, moves the system down to 0x0000, changes
; to protected mode, and calls the start of system. System then must
; RE-initialize the protected mode in it's own tables, and enable
; interrupts as needed.
;
; Ȼ��ʹ�� BIOS �жϽ�ϵͳ���ص� 0x10000 ����Ȼ�󣬻���������жϣ�
; ��ϵͳ�ƶ��� 0x0000 ������ϵͳ�л�������ģʽ������ϵͳ����������
; �������ø��ֱ�񣬳�ʼ������ģʽ��������Ҫ���жϡ�
;
; NOTE! currently system is at most 8*65536 bytes long. This should be no
; problem, even in the future. I want to keep it simple. This 512 kB
; kernel size should be enough - in fact more would mean we'd have to move
; not just these start-up routines, but also do something about the cache-
; memory (block IO devices). The area left over in the lower 640 kB is meant
; for these. No other memory is assumed to be "physical", ie all memory
; over 1Mb is demand-paging. All addresses under 1Mb are guaranteed to match
; their physical addresses.
;
; ע��:��ǰϵͳ�������� 8*65536 bytes��512K���������С��ʹ�ڽ���
; ҲӦ��û�����⡣���뱣��ϵͳ��С�ɡ�512K �Ĵ�СӦ���㹻�ˣ�ʵ����
; ����������ǲ�����Ҫ�����������򣬻���Ҫ��������ռ䣨Ϊ���豸�����͵�ַ
; �ռ�� 640K ����Ϊ�����Ŀ�ġ����������пռ�������ַ��ʵ�ʵ�ַ��Ӧ��
; ���磺���� 1M �Ŀռ���Ҫ��ҳ�����е��� 1M �Ŀռ���Ա�֤ʵ�ʵ�ַ�������ַ
; �Ķ�Ӧ��
;
; Ϊʲô��� 512K����Ϊһ����Ϊ 64K����ռ���� 8 ���Ρ��ھŸ������������롣
;
; NOTE1 abouve is no longer valid in it's entirety. cache-memory is allocated
; above the 1Mb mark as well as below. Otherwise it is mainly correct.
;
; ע��1:��������ȫ����ȷ�������ڴ�ȿ����� 1M �ռ�����Ҳ���������£�
; ��������ȷ�ġ�
;
; NOTE 2! The boot disk type must be set at compile-time, by setting
; the following equ. Having the boot-up procedure hunt for the right
; disk type is severe brain-damage.
; The loader has been made as simple as possible (had to, to get it
; in 512 bytes with the code to move to protected mode), and continuos
; read errors will result in a unbreakable loop. Reboot by hand. It
; loads pretty fast by getting whole sectors at a time whenever possible.
;
; ע��2:�������̵����ͱ����ڱ���ʱͨ����������ĳ��������á�
; ������������ӵ��Ѱ����ȷ�����豸�Ĵ��������ص��Բ���Ϊ��
; ���س��򱻾�����д�����С��Ӧ�������������л�������ģʽ�Ĵ���� 512K����
; ����������ȡ������������ѭ������Ҫ�ֶ�������ͨ������һ�ζ�ȡ�����ŵ���
; ��ʹ�����ٶȷǳ��졣
;

; 1.44Mb disks:
sectors = 18
; 1.2Mb disks:
; sectors = 15
; 720kB disks:
; sectors = 9

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

BOOTSEG = 0x07c0 ; �������򱻼��ص��ĵ�ַ
INITSEG = 0x9000 ; ���������Լ��ƶ��ĵ�ַ
SYSSEG  = 0x1000 ; ϵͳ�����ص� 0x10000 ��ʼ�ĶΣ��� 512K
ENDSEG	= SYSSEG + SYSSIZE ; ϵͳ����Ľ�����ַ������ SYSSIZE �� Makefile �ж���

entry start
start:
	mov	ax,#BOOTSEG ; ���������򱻼��صĵ�ַ�ƶ��� AX ��
	mov	ds,ax ; �����ݶεĻ�ַ����Ϊ�����������ʼ��ַ
	mov	ax,#INITSEG ; �������ᵽ��Ŀ���ַ���õ� AX ��
	mov	es,ax ; �� AX �е�ֵ���õ� ES �У�ʹ�û�ַ��ƫ�Ƶķ�ʽ�ƶ���ַ
	mov	cx,#256 ; �� CX ��������ֵ����Ϊ 256
	sub	si,si ; ��� SI
	sub	di,di ; ��� DI
	rep ; �ظ�ִ�� movw 256 �Σ�ʵ�����ǽ� 512K �����ݣ����������������Ƶ�INITSEG ��
	movw ; ����ָ�����Ѱַ��ʽ��Դ��DS:SI,Ŀ�꣺ES:DI
	jmpi	go,INITSEG ; jmpi �μ���תָ����ڵ�ǰ�����Ѿ������Ƶ� INITSEG ���ڲ�ͬ�Ķ�
go:	mov	ax,cs ; ������εĵ�ַ�ƶ��� AX���μ���ת��ı� CS
	mov	ds,ax ; �� AX ��ֵ�ƶ��� DS
	mov	es,ax ; �� AX ��ֵ�ƶ��� ES
	mov	ss,ax ; �� AX ��ֵ�ƶ��� SS����ջ�εĻ�ַ
	mov	sp,#0x400 ; ջ��ָ�룬��ջ�εĴ�С����Ϊ 512K����ջ���������·�չ������Ḳ�� INITSEG �Ĵ���

	mov	ah,#0x03 ; BIOS 10H �жϵ� 03H ���񣬶�ȡ��ǰ���λ�ã�dh���У�dl����
	xor	bh,bh ; ��� bh
	int	0x10 ; ִ�� 10H �ж�
	
	mov	cx,#24 ; �� CX ��ֵ����Ϊ 24����Ҫ��ʾ�ַ��ĸ���
	mov	bx,#0x0007 ; ������ʾ����	; page 0, attribute 7 (normal)
	mov	bp,#msg1 ; ��Ҫ��ʾ�ַ�����ʼ��ַ���ص� BP
	mov	ax,#0x1301 ; 13H��ʾ�ַ�����ES:BP=��ʾ����ַ��AL����ʾ�����ʽ��1���ַ�����ֻ����ʾ�ַ�������ʾ������BL�У���ʾ�󣬹��λ�øı䣩���ۺ����������ǣ�����Ļд�ַ������ƶ����
	int	0x10 ; ִ���ж�

; ok, we've written the message, now
; we want to load the system (at 0x10000)
;
; ��������Ļ�������Ϣ�����ڽ�ϵͳ���ص� 0x10000 ��

	mov	ax,#SYSSEG ; �� AX ��ֵ����Ϊ SYSSEG
	mov	es,ax ; �� ES ��ֵ����Ϊ SYSSEG
	call	read_it ; ���� read_it��call ָ���ʹ�ö�ջ�Ĵ���
	call	kill_motor ; ���� kill_motor���ر�����

; if the read went well we get current cursor position ans save it for
; posterity.
;
; �����ȡϵͳ�Ĺ���˳����ɣ����Ƕ�ȡ��ǰ����λ�ò����棬�Ա�����ʹ��

	mov	ah,#0x03	; read cursor pos
	xor	bh,bh
	int	0x10		; save it in known place, con_init fetches
	mov	[510],dx	; it from 0x90510.����ǰ���λ�ô洢��ϵͳ�������һ���ε���������ֽ�
		
; now we want to move to protected mode ...
;
; �������ǽ��뱣��ģʽ

	cli ; �ر��ж�

; first we move the system to it's rightful place

	mov	ax,#0x0000 ; AX ������ΪĿ��λ�ַ
	cld	; DF ����Ϊ 0��SI �� DI ����
do_move:
	mov	es,ax ; ����Ŀ��εĻ�ַ
	add	ax,#0x1000 ; ��Ŀ��εĻ�ַ��һ
	cmp	ax,#0x9000 ; ��Ŀ��εĻ�ַ�������ַ���бȽ�
	jz	end_move ; �����ȣ���������
	mov	ds,ax ; ��ǰϵͳ��ʼ�Ķ� 0x1000��0x10000 ���� 4 λ��
	sub	di,di ; di ����
	sub	si,si ; si ����
	mov 	cx,#0x8000 ; 64K
	rep
	movsw ; Դ��DS:SI,Ŀ�꣺ES:DI
	j	do_move

; then we load the segment descriptors
; ���ض�������

end_move:

	mov	ax,cs ; ����ɸ���ϵͳ�󣬴��뻹�ڵ�ǰ��ִ�У��ָ� DS ����ǰ�λ�ַ
	mov	ds,ax ;�� DS ����Ϊ��ȷ��ֵ
	;�������������Ŀ�ľ���Ϊ�˽��뱣��ģʽ�������뱣��ģʽ����������������ű�
	lidt	idt_48 ; �����ж��������� load idt with 0,0
	lgdt	gdt_48 ; ����ȫ���������� load gdt with whatever appropriate

; that was painless, now we enable A20

	call	empty_8042 ; �������뻺����Ϊ��
	mov	al,#0xD1 ; �Լ��̿���������������ͨ��д�˿�64hʵ�֣����� D1��׼��дOutput�˿ڡ����ͨ��60h�˿�д����ֽڣ��ᱻ������Output Port��
	out	#0x64,al ; ͨ��д�˿ڷ�������
	call	empty_8042
	mov	al,#0xDF ; A20 on,b7 b6 b5 b4 b3 b2 b1 b0, 0xDF Ϊ 1  1  0  1, 1  1  1  1, ���б���λb1����A20ѡͨ�ߡ�b5,b3,b2δʹ��
	out	#0x60,al
	call	empty_8042

; well, that went ok, I hope. Now we have to reprogram the interrupts :-(
; we put them right after the intel-reserved hardware interrupts, at
; int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
; messed this up with the original PC, and they haven't been able to
; rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
; which is used for the internal hardware interrupts as well. We just
; have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		; initialization sequence
	out	#0x20,al		; send it to 8259A-1
	.word	0x00eb,0x00eb		; jmp $+2, jmp $+2
	out	#0xA0,al		; and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		; start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		; start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		; 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		; 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		; 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		; mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

; well, that certainly wasn't fun :-(. Hopefully it works, and we don't
; need no steenking BIOS anyway (except for the initial loading :-).
; The BIOS-routine wants lots of unnecessary data, and it's less
; "interesting" anyway. This is how REAL programmers do it.
;
; Well, now's the time to actually move into protected mode. To make
; things as simple as possible, we do no register set-up or anything,
; we let the gnu-compiled 32-bit programs do that. We just jump to
; absolute address 0x00000, in 32-bit protected mode.

	mov	ax,#0x0001	; protected mode (PE) bit PE ��������Ϊ 1����Ҫ���뱣��ģʽ
	lmsw	ax		; lmsw��ָװ�����״̬�֣���ʵ�ʽ��뱣��ģʽ
	jmpi	0,8		; jmp offset 0 of segment 8 (cs)��Ҳ����ȫ����������ĵڶ��0 ��ַ��ʼ����

; This routine checks that the keyboard command queue is empty
; No timeout is used - if this hangs there is something wrong with
; the machine, and we probably couldn't proceed anyway.
; ����Ĵ�������̵���������ǿյġ���û��ʹ�ó�ʱ���ƣ��������Ĵ���
; ������˵�����������⣬���ǲ��ܴ����������
empty_8042:
	.word	0x00eb,0x00eb ; ����תָ�������
	in	al,#0x64	; 8042 status port �� 8042 ��״̬�ڶ�ȡ״̬��
	test	al,#2		; is input buffer full? �������뻺�����Ƿ�����
	jnz	empty_8042	; yes - loop ������ˣ�ѭ���ж�
	ret

; This routine loads the system at address 0x10000, making sure
; no 64kB boundaries are crossed. We try to load it as fast as
; possible, loading whole tracks whenever we can.
;
; in:	es - starting address segment (normally 0x1000)
;
; This routine has to be recompiled to fit another drive type,
; just change the "sectors" variable at the start of the file
; (originally 18, for a 1.44Mb drive)
; 
; ��ȡ���̣��ȶ�һ���ŵ����ٶ�ͬ�ŵ�����һ�����棬Ȼ���ظ�������̶�ȡ��һ���ŵ�
; 1.44Mb �����̽ṹ��2�桢80��/�桢18����/����512�ֽ�/������2880������512�ֽ�/����X 2880���� = 1440 KB ��ÿ���ŵ� 9K��ÿ����64K��7���ŵ�+2����������ע��������������
;
sread:	.word 1			; ��ǰ�ŵ��Ѿ���ȡ��������
head:	.word 0			; ��ǰ��ͷ��
track:	.word 0			; ��ǰ�ŵ���
read_it:
	mov ax,es ; ES ��ϵͳ����Ķλ�ַ����ǰ�ζ����� ES �ᱻ��һ��
	test ax,#0x0fff ; �൱�� AX �� 4 λ���㣬�� 4 λ����ԭ����ֵ
die:	jne die			; es must be at 64kB boundary ���������ƣ�Ҫ�����
	xor bx,bx		; BX ���㣬���������ʼ��ַ,�����ȡ���ݺ� BX ��仯
rp_read:
	mov ax,es ; �� ES ��ֵ���ص� AX ��
	cmp ax,#ENDSEG ; �� AX ��ֵ�������ַ���бȽϣ��ж��Ƿ���ɼ���
	jb ok1_read ; ���С��#ENDSEG����ת�� ok1_read
	ret ; ���̽��������أ�ͨ����ת���洢�ڶ�ջ�еķ��ص�ַ����ִ��
ok1_read: ;������û�������� #ENDSEG ʱִ��
	mov ax,#sectors ; �����������ص� AX �У�ʵ��Ϊ AL
	sub ax,sread ; �� AX ��ֵ��һ��ʵ��Ϊ AL�������������ʱ AL ����������
	mov cx,ax ; �� CX ��ֵ����Ϊ AX ��ֵ����ʱ AX �д�ŵ��ϴζ�ȡ��������
	shl cx,#9 ; 2 �� 9 �η��� 512��ʣ������������ 512Byte �����Ѿ���ȡ���ֽ���
	add cx,bx ; CX Ϊ 16 λ�Ĵ����������Ա�ʾ 64K ���ݣ�����Ѿ���ȡ���ֽ������ϻ�ַ��BX��������ǰ����Ҫ�������
	jnc ok2_read ; ��CFû����λ����ת������û������ͼ���ִ�У��˴���ʾ�����ȡ���������� 64K ��תִ��
	je ok2_read ; �������־ZF ����ת�ж��������˴���ʾ�����ȡ�����ݵ��� 64K ��תִ��
	xor ax,ax ; AX ���㣬���ִ�д˴����룬��ʾ��ȡ���������������ǰ�ŵ���ʣ���ֽ��������˵�ǰ����Ҫ���ֽ�
	sub ax,bx ; ����������൱��0xFFFF-BX�����ڻ���Ҫ��д���ֽ���
	shr ax,#9 ; AX/512;ÿ������ 512 Byte����ʱ AX ���ǻ���Ҫ��ȡ��������
ok2_read: ; ��û��������ǰ��ʱִ��
	call read_track ; ���� read_track
	mov cx,ax ; �� CX ����Ϊ AX��AX �д�ŵ����ϴζ�ȡ��������
	add ax,sread ; �� AX ��ֵ����Ϊ AX + sread = Total Sectors
	cmp ax,#sectors ; �ж��Ƿ��Ѿ�����
	jne ok3_read ; ���û�ж��꣬��ת�� ok3_read
	mov ax,#1 ; �� AX ����Ϊ 1�����е��˴��Ǵ���ǰ�ŵ��������Ѿ�����
	sub ax,head ; AX = AX - head
	jne ok4_read ; �����Ϊ 0����ǰ�����Ѿ����꣬��ת�� ok4_read
	inc track ; ���Ϊ 0����ǰ����û�ж��꣬���Ӵŵ��ţ���������һ���ŵ�
ok4_read: ; �ڵ�ǰ�������ʱִ��
	mov head,ax ; �� AX ��ֵ���浽 head ��
	xor ax,ax ; �� AX ���㣬����ִ��
ok3_read: ; �ڵ�ǰ�ŵ�û�ж���ʱִ��
	mov sread,ax ; ����һ����Ҫ��ȡ�Ĵŵ��ű��浽 sread ��
	shl cx,#9 ; �� CX ���� 512��CX ����ʣ���ȡ������������ʱ CX ����ʣ���ȡ���ֽ���
	add bx,cx ; BX = BX + CX����ʱ BX Ϊ�´δ�����ݵ���ʼ��ַ
	jnc rp_read ; ���û�г��� 64K ������
	mov ax,es ; ������� 64K���� ES���λ�ַ����ֵ���õ� AX ��
	add ax,#0x1000 ; AH ��һ
	mov es,ax ; �� ES ����Ϊ AX���ƶ�����һ����
	xor bx,bx ; ��� BX
	jmp rp_read ; ��������ǰ�ŵ�

; BIOS 13H��02H���ܣ����Ӵ����ϰ�һ���������������ݶ�������������Ϊ����һ���ͼ����ܣ���һ�������ж�ȡ��ȫ������������ͬһ���ŵ��ϣ���ͷ�źʹŵ�����ͬ��
; ��ڲ���: AH=02H ;���ܺ�
;          AL=������
;          CH��CL=�ŵ��ŵĵ�8λ����λ7-6��ʾ�ŵ��ŵĸ�2λ����6λ����������ʼ������
;          DH��DL=��ͷ�š���������
;          ES:BX=���ݻ�������ַ
; ����:    AH=0���ɹ���AL=��ȡ��������
;         ���CF=1��AX�д�ų���״̬��AH=������
; ע�⣺    �Ĵ���DS��BX��CX��DX����
; ��ͷ�ţ�  ����A��=0������B��=1
; �������ţ�����A=0������B=1��Ӳ��=80H

read_track:
	push ax
	push bx
	push cx
	push dx ; �����ֳ�����ʱ AL ����ʣ���ȡ����������ע�⣺����Ĵ����� DX ��Ϊ��ʱ���ݴ�Ŷ��Ĵ���
	mov dx,track ; �� DX ����Ϊ track���ŵ��ţ�����ʱ DL Ϊ�ŵ��ţ�DX ������ʱ����
	mov cx,sread ; �� CX ����Ϊ sread�������ţ�����ʱ CL Ϊ������
	inc cx ; CX �� 1����һ��ԭ�򣺴��̵ĵ�һ��������512K����ŵ����������룬����ϵͳ���룬ϵͳ����ӵڶ���������ʼ�����˵�һ�������� 1 ��ʼ��ȡ
	mov ch,dl ; �� CH ����Ϊ DL�����ŵ��ţ���ʱ CH Ϊ�ŵ��ţ��ŵ������������Ѿ��������
	mov dx,head ; DX ����Ϊ��ͷ�ţ��˴�Ϊ DL����ʱ DL Ϊ��ͷ�ţ�DL ������ʱ����
	mov dh,dl ; �� DH ͬ������Ϊ��ͷ�ţ���ʱ DH Ϊ��ͷ��
	mov dl,#0 ; �� DL ����Ϊ 0������A=0��DL Ϊ��������
	and dx,#0x0100 ; DX�е�ֵ���� DH �����һλ���������������㣨�����Ǿ�������ȥ��������DHΪ��ͷ��
	mov ah,#2 ; �� AH ����Ϊ 2�����ܺ�
	int 0x13 ; ���� BIOS 13H �ж�
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax ; �ָ��ֳ�
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13 ; ���̸�λ��13H��00H�Ź��ܡ�������ϵͳ��λ��AH=00H ���ܺţ�DL=�������ţ���֮��������A��λ
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track ; ��ת�� read_track�����������ȡ��ǰ���������쳣��������

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

gdt:
	;Selector 0x00 cannot be used
	.word	0,0,0,0		; dummy
	
	;Selector 0x08 will be our code(2*4=8)
	.word	0x07FF		; 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		; base address=0
	.word	0x9A00		; code read/exec
	.word	0x00C0		; granularity=4096, 386

	;Selector 0x10 will be our data(2*8=16)
	.word	0x07FF		; 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		; base address=0
	.word	0x9200		; data read/write
	.word	0x00C0		; granularity=4096, 386

idt_48:
	.word	0			; idt limit=0
	.word	0,0			; idt base=0L

gdt_48:
	.word	0x800		; gdt limit=2048, 256 GDT entries
	.word	gdt,0x9		; gdt base = 0X9xxxx,�������������� 0x9 ����Σ�����ȫ����������Ҳ�����������
	
msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.text
endtext:
.data
enddata:
.bss
endbss:
