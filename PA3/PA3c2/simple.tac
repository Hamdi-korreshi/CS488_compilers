comment start
label Main_main_0
t0 <- int 3
t1 <- int 5
t2 <- + t0 t1
t3 <- call 5 t2
t4 <- string 
\n
t5 <- call 6 t4
return t5
					## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
## return address handling
## store class tag, object size and vtable pointer
## initialize attributes
ret
.globl Main..new
Main..new:		constructor for Main
push %rbp
## stack room for temporaries: 1
subq $8, %rsp
movq 6, %rsi
movq $8, %rdi
call calloc
movq %rax, %r12
movq $10, %r12
movq 3, 8(%r12)
movq $Main..vtable, 16(%r12)
## self[3] holds field variable (type)
movq %rbp, %rsp
pop %rbp
.globl Main.main
Main.main:				## method definition
push %rbp
movq 16(%rsp), %r12
## stack room for temporaries: x
subq $16, %rsp
## method body begins
.globl Main.main.end
Main.main.end:		## method body ends
movq %rbp, %rsp
pop %rbp
sample text