;
;	boot.s
;
; boot.s is loaded at 0x7c00 by the bios-startup routines, and moves itself
; out of the way to address 0x90000, and jumps there.
;
; boot.s 编译后生成的代码会被 BIOS 加载到 0x7C00 的地址处，
; 然后会被自身移动到 0x90000 处执行。
;
; It then loads the system at 0x10000, using BIOS interrupts. Thereafter
; it disables all interrupts, moves the system down to 0x0000, changes
; to protected mode, and calls the start of system. System then must
; RE-initialize the protected mode in it's own tables, and enable
; interrupts as needed.
;
; 然后使用 BIOS 中断将系统加载到 0x10000 处。然后，会禁用所用中断，
; 将系统移动到 0x0000 处，将系统切换到保护模式，调用系统的启动程序，
; 重新设置各种表格，初始化保护模式，启用需要的中断。
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
; 注意:当前系统最大可以有 8*65536 bytes（512K）。这个大小即使在将来
; 也应该没有问题。我想保持系统的小巧。512K 的大小应该足够了，实际上
; 如果更大我们不仅需要复制启动程序，还需要保留缓存空间（为块设备）。低地址
; 空间的 640K 就是为了这个目的。并不是所有空间的物理地址与实际地址对应，
; 例如：大于 1M 的空间需要分页。所有低于 1M 的空间可以保证实际地址与物理地址
; 的对应。
;
; 为什么最大 512K？因为一个段为 64K，共占用了 8 个段。第九个段是引导代码。
;
; NOTE1 abouve is no longer valid in it's entirety. cache-memory is allocated
; above the 1Mb mark as well as below. Otherwise it is mainly correct.
;
; 注意1:上述不再全部正确。缓存内存既可以在 1M 空间以上也可以在以下，
; 其他是正确的。
;
; NOTE 2! The boot disk type must be set at compile-time, by setting
; the following equ. Having the boot-up procedure hunt for the right
; disk type is severe brain-damage.
; The loader has been made as simple as possible (had to, to get it
; in 512 bytes with the code to move to protected mode), and continuos
; read errors will result in a unbreakable loop. Reboot by hand. It
; loads pretty fast by getting whole sectors at a time whenever possible.
;
; 注意2:启动磁盘的类型必须在编译时通过设置下面的常量来设置。
; 在启动程序中拥有寻找正确启动设备的代码是严重的脑残行为。
; 加载程序被尽可能写得体积小（应该这样，包含切换到保护模式的代码才 512K），
; 并且连续读取，错误会造成死循环，需要手动重启。通过尽量一次读取整个磁道，
; 来使加载速度非常快。
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

BOOTSEG = 0x07c0 ; 引导程序被加载到的地址
INITSEG = 0x9000 ; 引导程序将自己移动的地址
SYSSEG  = 0x1000 ; 系统被加载到 0x10000 起始的段，共 512K
ENDSEG	= SYSSEG + SYSSIZE ; 系统代码的结束地址，其中 SYSSIZE 在 Makefile 中定义

entry start
start:
	mov	ax,#BOOTSEG ; 将引导程序被加载的地址移动到 AX 中
	mov	ds,ax ; 将数据段的基址设置为引导程序的起始地址
	mov	ax,#INITSEG ; 将上文提到的目标地址设置到 AX 中
	mov	es,ax ; 将 AX 中的值设置到 ES 中，使用基址加偏移的方式移动地址
	mov	cx,#256 ; 将 CX 计数器的值设置为 256
	sub	si,si ; 清空 SI
	sub	di,di ; 清空 DI
	rep ; 重复执行 movw 256 次，实际上是将 512K 的数据（引导程序自身）搬移到INITSEG 处
	movw ; 搬移指令，数据寻址方式，源：DS:SI,目标：ES:DI
	jmpi	go,INITSEG ; jmpi 段间跳转指令，由于当前代码已经被复制到 INITSEG ，在不同的段
go:	mov	ax,cs ; 将代码段的地址移动到 AX，段间跳转会改变 CS
	mov	ds,ax ; 将 AX 的值移动到 DS
	mov	es,ax ; 将 AX 的值移动到 ES
	mov	ss,ax ; 将 AX 的值移动到 SS，堆栈段的基址
	mov	sp,#0x400 ; 栈顶指针，堆栈段的大小设置为 512K。堆栈可能是向下发展，否则会覆盖 INITSEG 的代码

	mov	ah,#0x03 ; BIOS 10H 中断的 03H 服务，读取当前光标位置，dh：行，dl：列
	xor	bh,bh ; 清空 bh
	int	0x10 ; 执行 10H 中断
	
	mov	cx,#24 ; 将 CX 的值设置为 24，需要显示字符的个数
	mov	bx,#0x0007 ; 设置显示属性	; page 0, attribute 7 (normal)
	mov	bp,#msg1 ; 将要显示字符的起始地址加载到 BP
	mov	ax,#0x1301 ; 13H显示字符串（ES:BP=显示串地址）AL＝显示输出方式（1：字符串中只含显示字符，其显示属性在BL中，显示后，光标位置改变），综合起来功能是：向屏幕写字符串并移动光标
	int	0x10 ; 执行中断

; ok, we've written the message, now
; we want to load the system (at 0x10000)
;
; 我们向屏幕输出了信息，现在将系统加载到 0x10000 处

	mov	ax,#SYSSEG ; 将 AX 的值设置为 SYSSEG
	mov	es,ax ; 将 ES 的值设置为 SYSSEG
	call	read_it ; 调用 read_it，call 指令会使用堆栈寄存器
	call	kill_motor ; 调用 kill_motor，关闭软驱

; if the read went well we get current cursor position ans save it for
; posterity.
;
; 如果读取系统的过程顺利完成，我们读取当前光标的位置并保存，以备后来使用

	mov	ah,#0x03	; read cursor pos
	xor	bh,bh
	int	0x10		; save it in known place, con_init fetches
	mov	[510],dx	; it from 0x90510.将当前光标位置存储到系统代码最后一个段的最后两个字节
		
; now we want to move to protected mode ...
;
; 现在我们进入保护模式

	cli ; 关闭中断

; first we move the system to it's rightful place

	mov	ax,#0x0000 ; AX 被设置为目标段基址
	cld	; DF 被置为 0，SI 与 DI 增加
do_move:
	mov	es,ax ; 设置目标段的基址
	add	ax,#0x1000 ; 将目标段的基址加一
	cmp	ax,#0x9000 ; 将目标段的基址与结束基址进行比较
	jz	end_move ; 如果相等，结束复制
	mov	ds,ax ; 当前系统起始的段 0x1000（0x10000 右移 4 位）
	sub	di,di ; di 清零
	sub	si,si ; si 清零
	mov 	cx,#0x8000 ; 64K
	rep
	movsw ; 源：DS:SI,目标：ES:DI
	j	do_move

; then we load the segment descriptors
; 加载段描述符

end_move:

	mov	ax,cs ; 在完成复制系统后，代码还在当前段执行，恢复 DS 到当前段基址
	mov	ds,ax ;将 DS 设置为正确的值
	;设置这两个表的目的就是为了进入保护模式，当进入保护模式后会重新设置这两张表
	lidt	idt_48 ; 加载中断描述符表 load idt with 0,0
	lgdt	gdt_48 ; 加载全局描述符表 load gdt with whatever appropriate

; that was painless, now we enable A20

	call	empty_8042 ; 键盘输入缓冲区为空
	mov	al,#0xD1 ; 对键盘控制器发送命令是通过写端口64h实现，其中 D1：准备写Output端口。随后通过60h端口写入的字节，会被放置在Output Port中
	out	#0x64,al ; 通过写端口发出命令
	call	empty_8042
	mov	al,#0xDF ; A20 on,b7 b6 b5 b4 b3 b2 b1 b0, 0xDF 为 1  1  0  1, 1  1  1  1, 其中比特位b1连接A20选通线。b5,b3,b2未使用
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

	mov	ax,#0x0001	; protected mode (PE) bit PE 将被设置为 1，将要进入保护模式
	lmsw	ax		; lmsw是指装入机器状态字，即实际进入保护模式
	jmpi	0,8		; jmp offset 0 of segment 8 (cs)，也就是全局描述符表的第二项，0 地址开始运行

; This routine checks that the keyboard command queue is empty
; No timeout is used - if this hangs there is something wrong with
; the machine, and we probably couldn't proceed anyway.
; 下面的代码检查键盘的命令队列是空的。并没有使用超时限制，如果这里的代码
; 被挂起，说明机器有问题，我们不能处理这种情况
empty_8042:
	.word	0x00eb,0x00eb ; 空跳转指令，机器码
	in	al,#0x64	; 8042 status port 从 8042 的状态口读取状态码
	test	al,#2		; is input buffer full? 测试输入缓冲区是否已满
	jnz	empty_8042	; yes - loop 如果满了，循环判断
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
; 读取过程：先读一个磁道，再读同磁道的另一个盘面，然后重复上面过程读取下一个磁道
; 1.44Mb 的软盘结构：2面、80道/面、18扇区/道、512字节/扇区，2880扇区，512字节/扇区X 2880扇区 = 1440 KB ，每个磁道 9K，每个段64K（7个磁道+2个扇区），注意下面的溢出处理
;
sread:	.word 1			; 当前磁道已经读取的扇区数
head:	.word 0			; 当前磁头号
track:	.word 0			; 当前磁道号
read_it:
	mov ax,es ; ES 是系统代码的段基址（当前段读满后 ES 会被加一）
	test ax,#0x0fff ; 相当于 AX 高 4 位清零，低 4 位保持原来的值
die:	jne die			; es must be at 64kB boundary 按扇区复制，要求对齐
	xor bx,bx		; BX 清零，用来标记起始地址,后面读取数据后 BX 会变化
rp_read:
	mov ax,es ; 将 ES 的值加载到 AX 中
	cmp ax,#ENDSEG ; 将 AX 的值与结束地址进行比较，判断是否完成加载
	jb ok1_read ; 如果小于#ENDSEG，跳转到 ok1_read
	ret ; 过程结束，返回，通过跳转到存储在堆栈中的返回地址继续执行
ok1_read: ;在数据没有填满到 #ENDSEG 时执行
	mov ax,#sectors ; 将扇区数加载到 AX 中，实际为 AL
	sub ax,sread ; 将 AX 的值减一，实际为 AL，在下面读磁盘时 AL 代表扇区数
	mov cx,ax ; 将 CX 的值设置为 AX 的值，此时 AX 中存放的上次读取的扇区数
	shl cx,#9 ; 2 的 9 次方是 512，剩余扇区数乘以 512Byte 等于已经读取的字节数
	add cx,bx ; CX 为 16 位寄存器，最多可以表示 64K 数据，如果已经读取的字节数加上基址（BX）超过当前段需要溢出处理
	jnc ok2_read ; 若CF没有置位则跳转，即：没有溢出就继续执行，此处表示如果读取的数据少于 64K 跳转执行
	je ok2_read ; 利用零标志ZF 作跳转判断条件，此处表示如果读取的数据等于 64K 跳转执行
	xor ax,ax ; AX 清零，如果执行此处代码，表示读取发生了溢出即：当前磁道的剩余字节数超过了当前段需要的字节
	sub ax,bx ; 发生溢出，相当于0xFFFF-BX，等于还需要填写的字节数
	shr ax,#9 ; AX/512;每个扇区 512 Byte，此时 AX 中是还需要读取的扇区数
ok2_read: ; 在没有填满当前段时执行
	call read_track ; 调用 read_track
	mov cx,ax ; 将 CX 设置为 AX，AX 中存放的是上次读取的扇区数
	add ax,sread ; 将 AX 的值设置为 AX + sread = Total Sectors
	cmp ax,#sectors ; 判断是否已经读完
	jne ok3_read ; 如果没有读完，跳转到 ok3_read
	mov ax,#1 ; 将 AX 设置为 1，运行到此处是代表当前磁道的扇区已经读完
	sub ax,head ; AX = AX - head
	jne ok4_read ; 如果不为 0，当前盘面已经读完，跳转到 ok4_read
	inc track ; 如果为 0，当前盘面没有读完，增加磁道号，继续读下一个磁道
ok4_read: ; 在当前盘面读完时执行
	mov head,ax ; 将 AX 的值保存到 head 中
	xor ax,ax ; 将 AX 清零，继续执行
ok3_read: ; 在当前磁道没有读完时执行
	mov sread,ax ; 将下一个需要读取的磁道号保存到 sread 中
	shl cx,#9 ; 将 CX 乘以 512，CX 代表剩余读取的扇区数，此时 CX 代表剩余读取的字节数
	add bx,cx ; BX = BX + CX，此时 BX 为下次存放数据的起始地址
	jnc rp_read ; 如果没有超过 64K 继续读
	mov ax,es ; 如果超过 64K，将 ES（段基址）的值设置到 AX 中
	add ax,#0x1000 ; AH 加一
	mov es,ax ; 将 ES 设置为 AX，移动到下一个段
	xor bx,bx ; 清空 BX
	jmp rp_read ; 继续读当前磁道

; BIOS 13H的02H功能：将从磁盘上把一个或更多的扇区内容读进存贮器。因为这是一个低级功能，在一个操作中读取的全部扇区必须在同一条磁道上（磁头号和磁道号相同）
; 入口参数: AH=02H ;功能号
;          AL=扇区数
;          CH、CL=磁道号的低8位数、位7-6表示磁道号的高2位，低6位放入所读起始扇区号
;          DH、DL=磁头号、驱动器号
;          ES:BX=数据缓冲区地址
; 返回:    AH=0：成功，AL=读取的扇区数
;         如果CF=1，AX中存放出错状态：AH=错误码
; 注意：    寄存器DS、BX、CX、DX不变
; 磁头号：  软盘A面=0；软盘B面=1
; 驱动器号：软驱A=0；软驱B=1；硬驱=80H

read_track:
	push ax
	push bx
	push cx
	push dx ; 保护现场，此时 AL 等于剩余读取的扇区数，注意：下面的代码中 DX 作为临时数据存放饿寄存器
	mov dx,track ; 将 DX 设置为 track（磁道号），此时 DL 为磁道号，DX 中是临时数据
	mov cx,sread ; 将 CX 设置为 sread（扇区号），此时 CL 为扇区号
	inc cx ; CX 加 1，加一的原因：磁盘的第一个扇区（512K）存放的是引导代码，不是系统代码，系统代码从第二个扇区开始，除了第一次其它从 1 开始读取
	mov ch,dl ; 将 CH 设置为 DL，即磁道号，此时 CH 为磁道号－磁道号与扇区号已经设置完成
	mov dx,head ; DX 设置为磁头号，此处为 DL，此时 DL 为磁头号，DL 中是临时数据
	mov dh,dl ; 将 DH 同样设置为磁头号，此时 DH 为磁头号
	mov dl,#0 ; 将 DL 设置为 0，软驱A=0，DL 为驱动器号
	and dx,#0x0100 ; DX中的值除了 DH 的最后一位被保留，其它清零（上面那句代码可以去掉？），DH为磁头号
	mov ah,#2 ; 将 AH 设置为 2，功能号
	int 0x13 ; 调用 BIOS 13H 中断
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax ; 恢复现场
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13 ; 软盘复位，13H的00H号功能——软盘系统复位，AH=00H 功能号，DL=驱动器号，总之：将软驱A复位
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track ; 跳转到 read_track，即：如果读取当前扇区出现异常，继续读

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
	.word	gdt,0x9		; gdt base = 0X9xxxx,由于引导代码在 0x9 这个段，所以全局描述符表也就在这个段了
	
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
