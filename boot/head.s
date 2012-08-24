/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 * 注意：启动程序在 0x00000000，同时这个地址也是页表的地址，在这个启动完成后，这里的程序与数据会被页表覆盖。
 */
.code32 .text #代码段声明
.globl idt, gdt, pg_dir, startup_32 #全局符号，可以被其他编译单元引用

pg_dir:
startup_32:
	movl $0x10,%eax # 已经处于32 位保护模式，$0x10 是全局描述符表项中数据段的选择符
	mov %ax,%ds #ds=0x10
	mov %ax,%es #es=0x10
	mov %ax,%fs #fs=0x10
	mov %ax,%gs #gs=0x10
	lss stack_start,%esp # 设置系统堆栈：ss，esp，stack_start 在 kernel/sched.c, 40 行
	
	call setup_idt # 设置中断描述符表
	call setup_gdt # 设置全局描述符表
	
	# 因为修改了gdt，所以需要重新装载所有的段寄存器
	movl $0x10,%eax	# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp
	
	xorl %eax,%eax
	
	# 测试A20 地址线是否已经开启,如果没有开启，内核无法使用大于 1M 的内存
	# 采用的方法是向内存地址0x000000 处写入任意数值，
	# 检查 0x100000(1M) 处是否也是这个数值。
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000
	cmpl %eax,0x100000
	je 1b # 向后寻找标号
	
	# CR0 : http://en.wikipedia.org/wiki/Control_register
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,ET,PE
	testl $0x10,%eax # test MP 位
	jne 1f			# ET is set - 387 is present
	orl $4,%eax		# else set emulate bit, EM 位
1:	movl %eax,%cr0
	jmp after_page_tables

/*********************************setup_idt*******************************/
/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */

/*
 * 下面这段是设置中断描述符表子程序 setup_idt
 *
 * 将中断描述符表idt 设置成具有256 个项，并都指向ignore_int 中断门。然后加载中断
 * 描述符表寄存器(用lidt 指令)。真正使用的中断门以后再安装。当我们在其它地方认为一切
 * 都正常时再开启中断。该子程序将会被页表覆盖掉。
*/

setup_idt:
	lea ignore_int,%edx # 将 ignore_int 的有效地址（偏移值）存入到 edx 寄存器
	movl $0x00080000,%eax # 将选择符 0x0008 置入 eax 的高16 位中
	movw %dx,%ax		/* selector = 0x0008 = cs # 偏移值的低16 位置入eax 的低16 位中。此时eax 含有门描述符低4 字节的值*/
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea idt,%edi # 将 idt 的有效地址存入 edi
	mov $256,%ecx # 循环计数 256,256 个表项
rp_sidt:
	movl %eax,(%edi) # 将 eax 的值存入 idt 的第一项的前32位
	movl %edx,4(%edi) # 将 edx 的值存入 idt 的第一项的第二个32位
	addl $8,%edi # 使 edi 指向 idt 的第二项
	dec %ecx
	jne rp_sidt # 重复执行
	lidt idt_descr # 设置中断描述符表
	ret

/*********************************setup_gdt*******************************/
/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr # 设置全局描述附表
	ret

/**********************************************************************/
/* Linus 将内核的内存页表直接放在页目录之后，使用了2 个表来寻址8 Mb 的物理内存。
 * 如果你有多于8 Mb 的内存，就需要在这里进行扩充修改。
*/
# 每个页表长为4 Kb 字节，而每个页表项需要4 个字节，因此一个页表共可以存放1000 个表项，
# 如果一个表项寻址4 Kb 的地址空间，则一个页表就可以寻址4 Mb 的物理内存。
# 页表项的格式为：项的前0-11 位存放一些标志，如是否在内存中(P 位0)、读写许可(R/W 位1)、
# 普通用户还是超级用户使用(U/S 位2)、是否修改过(是否脏了)(D 位6)等；表项的位12-31 是
# 页框地址，用于指出一页内存的物理起始地址。

# 从偏移0x1000 处开始是第1 个页表（偏移0 开始处将存放页表目录）。

/*********************************0x1000*******************************/
.org 0x1000 # 设置起始地址
pg0:

/*********************************0x2000*******************************/
.org 0x2000
pg1:

/*********************************0x3000*******************************/
.org 0x3000
pg2:		# This is not used yet, but if you
			# want to expand past 8 Mb, you'll have
			# to use it.

/*********************************0x4000*******************************/
.org 0x4000
after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $main
	jmp setup_paging # 跳转到分页的设置代码
L6:
	jmp L6		# main should never return here, but
				# just in case, we know what happens.

/*********************************default interrupt "handler"*******************************/
/* This is the default interrupt "handler" :-) */
.align 2 # 2字节对齐
ignore_int:
	incb 0xb8000+160		# put something on the screen
	movb $2,0xb8000+161		# so that we know something
	iret					# happened

/*********************************Setup_paging*******************************/
/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 8MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 8 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 8Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "8Mb"), but I
 * won't guarantee that's all :-( )
 */
 
/*
 * 这个子程序通过设置控制寄存器cr0 的标志（PG 位31）来启动对内存的分页处理功能，
 * 并设置各个页表项的内容，以恒等映射前8 MB 的物理内存。分页器假定不会产生非法的
 * 地址映射（也即在只有4Mb 的机器上设置出大于4Mb 的内存地址）。
 * 注意！尽管所有的物理地址都应该由这个子程序进行恒等映射，但只有内核页面管理函数能
 * 直接使用>1Mb 的地址。所有"一般"函数仅使用低于1Mb 的地址空间，或者是使用局部数据
 * 空间，地址空间将被映射到其它一些地方去 -- mm(内存管理程序)会管理这些事的。
 * 对于那些有多于8Mb 内存的家伙 - 太幸运了，我还没有，为什么你会有?代码就在这里，
 * 对它进行修改吧。（实际上，这并不太困难的。通常只需修改一些常数等。我把它设置为
 * 8Mb，因为我的机器再怎么扩充甚至不能超过这个界限（当然，我的机器很便宜的）。
 * 我已经通过设置某类标志来给出需要改动的地方（搜索"8Mb"），但我不能保证作这些
 * 改动就行了）。
 */

# paging : http://en.wikipedia.org/wiki/Paging

.align 2
setup_paging:
	movl $1024*3,%ecx # 0x1000=4096 byte=1024*32位, pg2 并没有使用，也没有在这里初始化。
	xorl %eax,%eax # eax 清零
	xorl %edi,%edi # edi 清零/* pg_dir is at 0x000 */
	cld;rep;stosl # cld 设置传送方向，rep 重复 ecx 次，stos 将 eax 的值传送到 edi 所指向的内存
	
	# 下面2 句设置页目录中的项，我们共有2 个页表所以只需设置2 项。
	# 页目录项的结构与页表中项的结构一样，4 个字节为1 项。
	# "$pg0+7"表示：0x00001007，是页目录表中的第1 项。
	# 则第1 个页表所在的地址 = 0x00001007 & 0xfffff000 = 0x1000；
	# 第1 个页表的属性标志 = 0x00001007 & 0x00000fff = 0x07，表示该页存在、用户可读写。
	movl $pg0+7,pg_dir		/* set present bit/user r/w */
	movl $pg1+7,pg_dir+4	/*  --------- " " --------- */
	
	# 下面6 行填写2 个页表中所有项的内容，共有：2(页表)*1024(项/页表)=2048 项(0 - 0xfff)，
	# 也即能映射物理内存 2048*4Kb = 8Mb。
	# 每项的内容是：当前项所映射的物理内存地址 + 该页的标志（这里均为7）。
	# 使用的方法是从最后一个页表的最后一项开始按倒退顺序填写。一个页表的最后一项在页表中的
	# 位置是1023*4 = 4092。因此最后一页的最后一项的位置就是$pg3+4092。
	movl $pg1+4092,%edi
	# 最后1 项对应物理内存页面的地址是0x7ff000，
	# 加上属性标志7，即为0xfff007.
	movl $0x7ff007,%eax		/*  8Mb - 4096 + 7 (r/w user,p) */
	std # 方向位置位，edi 值递减(4 字节)。
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax # 每填写好一项，物理地址值减0x1000。
	jge 1b # 如果大于等于零，向后跳转到 1 继续执行。
	xorl %eax,%eax		/* pg_dir is at 0x0000 */  # 页目录表在0x0000 处
	movl %eax,%cr3		/* cr3 - page directory start */
	# 设置启动使用分页处理（cr0 的PG 标志，位31）
	movl %cr0,%eax
	orl $0x80000000,%eax 
	movl %eax,%cr0		/* set paging (PG) bit */ # 添上PG 标志
	
	# 在改变分页处理标志后要求使用转移指令刷新预取指令队列，这里用的是返回指令ret。
	# 该返回指令的另一个作用是将堆栈中的main 程序的地址弹出，并开始运行/init/main.c 程序。
	# 本程序到此真正结束了。终于结束了该死的汇编代码！
	ret			/* this also flushes prefetch-queue */

/*********************************idt_descr*******************************/
.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long idt

/*********************************gdt_descr*******************************/
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long gdt			# magic number, but it works for me :^)

/*********************************idt*******************************/
.align 8
idt:	.fill 256,8,0	# idt is uninitialized # 256 项，每项8 字节，填0。

/*********************************gdt*******************************/
gdt:
	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb , 代码段*/
	.quad 0x00c09200000007ff	/* 8Mb , 数据段*/
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
