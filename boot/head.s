/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 * ע�⣺���������� 0x00000000��ͬʱ�����ַҲ��ҳ��ĵ�ַ�������������ɺ�����ĳ��������ݻᱻҳ���ǡ�
 */
.code32 .text #���������
.globl idt, gdt, pg_dir, startup_32 #ȫ�ַ��ţ����Ա��������뵥Ԫ����

pg_dir:
startup_32:
	movl $0x10,%eax # �Ѿ�����32 λ����ģʽ��$0x10 ��ȫ�����������������ݶε�ѡ���
	mov %ax,%ds #ds=0x10
	mov %ax,%es #es=0x10
	mov %ax,%fs #fs=0x10
	mov %ax,%gs #gs=0x10
	lss stack_start,%esp # ����ϵͳ��ջ��ss��esp��stack_start �� kernel/sched.c, 40 ��
	
	call setup_idt # �����ж���������
	call setup_gdt # ����ȫ����������
	
	# ��Ϊ�޸���gdt��������Ҫ����װ�����еĶμĴ���
	movl $0x10,%eax	# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp
	
	xorl %eax,%eax
	
	# ����A20 ��ַ���Ƿ��Ѿ�����,���û�п������ں��޷�ʹ�ô��� 1M ���ڴ�
	# ���õķ��������ڴ��ַ0x000000 ��д��������ֵ��
	# ��� 0x100000(1M) ���Ƿ�Ҳ�������ֵ��
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000
	cmpl %eax,0x100000
	je 1b # ���Ѱ�ұ��
	
	# CR0 : http://en.wikipedia.org/wiki/Control_register
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,ET,PE
	testl $0x10,%eax # test MP λ
	jne 1f			# ET is set - 387 is present
	orl $4,%eax		# else set emulate bit, EM λ
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
 * ��������������ж����������ӳ��� setup_idt
 *
 * ���ж���������idt ���óɾ���256 �������ָ��ignore_int �ж��š�Ȼ������ж�
 * ��������Ĵ���(��lidt ָ��)������ʹ�õ��ж����Ժ��ٰ�װ���������������ط���Ϊһ��
 * ������ʱ�ٿ����жϡ����ӳ��򽫻ᱻҳ���ǵ���
*/

setup_idt:
	lea ignore_int,%edx # �� ignore_int ����Ч��ַ��ƫ��ֵ�����뵽 edx �Ĵ���
	movl $0x00080000,%eax # ��ѡ��� 0x0008 ���� eax �ĸ�16 λ��
	movw %dx,%ax		/* selector = 0x0008 = cs # ƫ��ֵ�ĵ�16 λ����eax �ĵ�16 λ�С���ʱeax ��������������4 �ֽڵ�ֵ*/
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea idt,%edi # �� idt ����Ч��ַ���� edi
	mov $256,%ecx # ѭ������ 256,256 ������
rp_sidt:
	movl %eax,(%edi) # �� eax ��ֵ���� idt �ĵ�һ���ǰ32λ
	movl %edx,4(%edi) # �� edx ��ֵ���� idt �ĵ�һ��ĵڶ���32λ
	addl $8,%edi # ʹ edi ָ�� idt �ĵڶ���
	dec %ecx
	jne rp_sidt # �ظ�ִ��
	lidt idt_descr # �����ж���������
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
	lgdt gdt_descr # ����ȫ����������
	ret

/**********************************************************************/
/* Linus ���ں˵��ڴ�ҳ��ֱ�ӷ���ҳĿ¼֮��ʹ����2 ������Ѱַ8 Mb �������ڴ档
 * ������ж���8 Mb ���ڴ棬����Ҫ��������������޸ġ�
*/
# ÿ��ҳ��Ϊ4 Kb �ֽڣ���ÿ��ҳ������Ҫ4 ���ֽڣ����һ��ҳ�����Դ��1000 �����
# ���һ������Ѱַ4 Kb �ĵ�ַ�ռ䣬��һ��ҳ��Ϳ���Ѱַ4 Mb �������ڴ档
# ҳ����ĸ�ʽΪ�����ǰ0-11 λ���һЩ��־�����Ƿ����ڴ���(P λ0)����д���(R/W λ1)��
# ��ͨ�û����ǳ����û�ʹ��(U/S λ2)���Ƿ��޸Ĺ�(�Ƿ�����)(D λ6)�ȣ������λ12-31 ��
# ҳ���ַ������ָ��һҳ�ڴ��������ʼ��ַ��

# ��ƫ��0x1000 ����ʼ�ǵ�1 ��ҳ��ƫ��0 ��ʼ�������ҳ��Ŀ¼����

/*********************************0x1000*******************************/
.org 0x1000 # ������ʼ��ַ
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
	jmp setup_paging # ��ת����ҳ�����ô���
L6:
	jmp L6		# main should never return here, but
				# just in case, we know what happens.

/*********************************default interrupt "handler"*******************************/
/* This is the default interrupt "handler" :-) */
.align 2 # 2�ֽڶ���
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
 * ����ӳ���ͨ�����ÿ��ƼĴ���cr0 �ı�־��PG λ31�����������ڴ�ķ�ҳ�����ܣ�
 * �����ø���ҳ��������ݣ��Ժ��ӳ��ǰ8 MB �������ڴ档��ҳ���ٶ���������Ƿ���
 * ��ַӳ�䣨Ҳ����ֻ��4Mb �Ļ��������ó�����4Mb ���ڴ��ַ����
 * ע�⣡�������е������ַ��Ӧ��������ӳ�����к��ӳ�䣬��ֻ���ں�ҳ���������
 * ֱ��ʹ��>1Mb �ĵ�ַ������"һ��"������ʹ�õ���1Mb �ĵ�ַ�ռ䣬������ʹ�þֲ�����
 * �ռ䣬��ַ�ռ佫��ӳ�䵽����һЩ�ط�ȥ -- mm(�ڴ�������)�������Щ�µġ�
 * ������Щ�ж���8Mb �ڴ�ļһ� - ̫�����ˣ��һ�û�У�Ϊʲô�����?����������
 * ���������޸İɡ���ʵ���ϣ��Ⲣ��̫���ѵġ�ͨ��ֻ���޸�һЩ�����ȡ��Ұ�������Ϊ
 * 8Mb����Ϊ�ҵĻ�������ô�����������ܳ���������ޣ���Ȼ���ҵĻ����ܱ��˵ģ���
 * ���Ѿ�ͨ������ĳ���־��������Ҫ�Ķ��ĵط�������"8Mb"�������Ҳ��ܱ�֤����Щ
 * �Ķ������ˣ���
 */

# paging : http://en.wikipedia.org/wiki/Paging

.align 2
setup_paging:
	movl $1024*3,%ecx # 0x1000=4096 byte=1024*32λ, pg2 ��û��ʹ�ã�Ҳû���������ʼ����
	xorl %eax,%eax # eax ����
	xorl %edi,%edi # edi ����/* pg_dir is at 0x000 */
	cld;rep;stosl # cld ���ô��ͷ���rep �ظ� ecx �Σ�stos �� eax ��ֵ���͵� edi ��ָ����ڴ�
	
	# ����2 ������ҳĿ¼�е�����ǹ���2 ��ҳ������ֻ������2 �
	# ҳĿ¼��Ľṹ��ҳ������Ľṹһ����4 ���ֽ�Ϊ1 �
	# "$pg0+7"��ʾ��0x00001007����ҳĿ¼���еĵ�1 �
	# ���1 ��ҳ�����ڵĵ�ַ = 0x00001007 & 0xfffff000 = 0x1000��
	# ��1 ��ҳ������Ա�־ = 0x00001007 & 0x00000fff = 0x07����ʾ��ҳ���ڡ��û��ɶ�д��
	movl $pg0+7,pg_dir		/* set present bit/user r/w */
	movl $pg1+7,pg_dir+4	/*  --------- " " --------- */
	
	# ����6 ����д2 ��ҳ��������������ݣ����У�2(ҳ��)*1024(��/ҳ��)=2048 ��(0 - 0xfff)��
	# Ҳ����ӳ�������ڴ� 2048*4Kb = 8Mb��
	# ÿ��������ǣ���ǰ����ӳ��������ڴ��ַ + ��ҳ�ı�־�������Ϊ7����
	# ʹ�õķ����Ǵ����һ��ҳ������һ�ʼ������˳����д��һ��ҳ������һ����ҳ���е�
	# λ����1023*4 = 4092��������һҳ�����һ���λ�þ���$pg3+4092��
	movl $pg1+4092,%edi
	# ���1 ���Ӧ�����ڴ�ҳ��ĵ�ַ��0x7ff000��
	# �������Ա�־7����Ϊ0xfff007.
	movl $0x7ff007,%eax		/*  8Mb - 4096 + 7 (r/w user,p) */
	std # ����λ��λ��edi ֵ�ݼ�(4 �ֽ�)��
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax # ÿ��д��һ������ֵַ��0x1000��
	jge 1b # ������ڵ����㣬�����ת�� 1 ����ִ�С�
	xorl %eax,%eax		/* pg_dir is at 0x0000 */  # ҳĿ¼����0x0000 ��
	movl %eax,%cr3		/* cr3 - page directory start */
	# ��������ʹ�÷�ҳ����cr0 ��PG ��־��λ31��
	movl %cr0,%eax
	orl $0x80000000,%eax 
	movl %eax,%cr0		/* set paging (PG) bit */ # ����PG ��־
	
	# �ڸı��ҳ�����־��Ҫ��ʹ��ת��ָ��ˢ��Ԥȡָ����У������õ��Ƿ���ָ��ret��
	# �÷���ָ�����һ�������ǽ���ջ�е�main ����ĵ�ַ����������ʼ����/init/main.c ����
	# �����򵽴����������ˡ����ڽ����˸����Ļ����룡
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
idt:	.fill 256,8,0	# idt is uninitialized # 256 �ÿ��8 �ֽڣ���0��

/*********************************gdt*******************************/
gdt:
	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb , �����*/
	.quad 0x00c09200000007ff	/* 8Mb , ���ݶ�*/
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
