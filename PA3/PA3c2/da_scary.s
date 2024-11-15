                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bar..vtable
Bar..vtable:            ## virtual function table for Bar
                        .quad string1
                        .quad Bar..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad IO.in_int
                        .quad IO.in_string
                        .quad IO.out_int
                        .quad IO.out_string
                        .quad Bazz.printh
                        .quad Foo.doh
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bazz..vtable
Bazz..vtable:           ## virtual function table for Bazz
                        .quad string2
                        .quad Bazz..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad IO.in_int
                        .quad IO.in_string
                        .quad IO.out_int
                        .quad IO.out_string
                        .quad Bazz.printh
                        .quad Bazz.doh
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bool..vtable
Bool..vtable:           ## virtual function table for Bool
                        .quad string3
                        .quad Bool..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Foo..vtable
Foo..vtable:            ## virtual function table for Foo
                        .quad string4
                        .quad Foo..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad IO.in_int
                        .quad IO.in_string
                        .quad IO.out_int
                        .quad IO.out_string
                        .quad Bazz.printh
                        .quad Foo.doh
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..vtable
IO..vtable:             ## virtual function table for IO
                        .quad string5
                        .quad IO..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad IO.in_int
                        .quad IO.in_string
                        .quad IO.out_int
                        .quad IO.out_string
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..vtable
Int..vtable:            ## virtual function table for Int
                        .quad string6
                        .quad Int..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..vtable
Main..vtable:           ## virtual function table for Main
                        .quad string7
                        .quad Main..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad Main.main
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object..vtable
Object..vtable:         ## virtual function table for Object
                        .quad string8
                        .quad Object..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Razz..vtable
Razz..vtable:           ## virtual function table for Razz
                        .quad string9
                        .quad Razz..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad IO.in_int
                        .quad IO.in_string
                        .quad IO.out_int
                        .quad IO.out_string
                        .quad Bazz.printh
                        .quad Foo.doh
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String..vtable
String..vtable:         ## virtual function table for String
                        .quad string10
                        .quad String..new
                        .quad Object.abort
                        .quad Object.copy
                        .quad Object.type_name
                        .quad String.concat
                        .quad String.length
                        .quad String.substr
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bar..new
Bar..new:               ## constructor for Bar
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 2
                        movq $16, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $12, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $10, %r14
                        movq %r14, 0(%r12)
                        movq $12, %r14
                        movq %r14, 8(%r12)
                        movq $Bar..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field h (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] holds field g (Foo)
                        movq $0, %r13
                        movq %r13, 32(%r12)
                        ## self[5] holds field i (Object)
                        movq $0, %r13
                        movq %r13, 40(%r12)
                        ## self[6] holds field a (Razz)
                        movq $0, %r13
                        movq %r13, 48(%r12)
                        ## self[7] holds field b (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 56(%r12)
                        ## self[8] holds field e (Bar)
                        movq $0, %r13
                        movq %r13, 64(%r12)
                        ## self[9] holds field f (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 72(%r12)
                        ## self[10] holds field c (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 80(%r12)
                        ## self[11] holds field d (Object)
                        movq $0, %r13
                        movq %r13, 88(%r12)
                        ## self[3] h initializer <- 1
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## self[4] g initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l1
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Bazz will jump to l2
                        ## case Razz will jump to l3
                        ## case Foo will jump to l4
                        ## case Bar will jump to l5
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l5
                        movq $11, %r14
                        cmpq %r14, %r13
			je l2
                        movq $0, %r14
                        cmpq %r14, %r13
			je l6
                        movq $12, %r14
                        cmpq %r14, %r13
			je l4
                        movq $13, %r14
                        cmpq %r14, %r13
			je l6
                        movq $1, %r14
                        cmpq %r14, %r13
			je l6
                        movq $14, %r14
                        cmpq %r14, %r13
			je l6
                        movq $15, %r14
                        cmpq %r14, %r13
			je l6
                        movq $16, %r14
                        cmpq %r14, %r13
			je l3
                        movq $3, %r14
                        cmpq %r14, %r13
			je l6
.globl l6
l6:                     ## case expression: error case
                        movq $string11, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l1
l1:                     ## case expression: void case
                        movq $string12, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l2
l2:                     ## fp[0] holds case n (Bazz)
                        ## new Foo
                        pushq %rbp
                        pushq %r12
                        movq $Foo..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l7
.globl l3
l3:                     ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l7
.globl l4
l4:                     ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l7
.globl l5
l5:                     ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l7
.globl l7
l7:                     ## case expression ends
                        movq %r13, 32(%r12)
                        ## self[5] i initializer <- printh(...)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 40(%r12)
                        ## self[6] a initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l8
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Razz will jump to l9
                        ## case Foo will jump to l10
                        ## case Bar will jump to l11
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l11
                        movq $11, %r14
                        cmpq %r14, %r13
			je l12
                        movq $0, %r14
                        cmpq %r14, %r13
			je l12
                        movq $12, %r14
                        cmpq %r14, %r13
			je l10
                        movq $13, %r14
                        cmpq %r14, %r13
			je l12
                        movq $1, %r14
                        cmpq %r14, %r13
			je l12
                        movq $14, %r14
                        cmpq %r14, %r13
			je l12
                        movq $15, %r14
                        cmpq %r14, %r13
			je l12
                        movq $16, %r14
                        cmpq %r14, %r13
			je l9
                        movq $3, %r14
                        cmpq %r14, %r13
			je l12
.globl l12
l12:                    ## case expression: error case
                        movq $string13, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l8
l8:                     ## case expression: void case
                        movq $string14, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l9
l9:                     ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l13
.globl l10
l10:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l13
.globl l11
l11:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l13
.globl l13
l13:                    ## case expression ends
                        movq %r13, 48(%r12)
                        ## self[7] b initializer <- a.doh(...) + g.doh(...) + doh(...) + printh(...)
                        ## a.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## a
                        movq 48(%r12), %r13
                        cmpq $0, %r13
			jne l14
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l14
l14:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Razz
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## g.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## g
                        movq 32(%r12), %r13
                        cmpq $0, %r13
			jne l15
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l15
l15:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Foo
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 56(%r12)
                        ## self[8] e initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l16
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Razz will jump to l17
                        ## case Bar will jump to l18
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l18
                        movq $11, %r14
                        cmpq %r14, %r13
			je l19
                        movq $0, %r14
                        cmpq %r14, %r13
			je l19
                        movq $12, %r14
                        cmpq %r14, %r13
			je l19
                        movq $13, %r14
                        cmpq %r14, %r13
			je l19
                        movq $1, %r14
                        cmpq %r14, %r13
			je l19
                        movq $14, %r14
                        cmpq %r14, %r13
			je l19
                        movq $15, %r14
                        cmpq %r14, %r13
			je l19
                        movq $16, %r14
                        cmpq %r14, %r13
			je l17
                        movq $3, %r14
                        cmpq %r14, %r13
			je l19
.globl l19
l19:                    ## case expression: error case
                        movq $string16, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l16
l16:                    ## case expression: void case
                        movq $string17, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l17
l17:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l20
.globl l18
l18:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l20
.globl l20
l20:                    ## case expression ends
                        movq %r13, 64(%r12)
                        ## self[9] f initializer <- a@Bazz.doh(...) + g.doh(...) + e.doh(...) + doh(...) + printh(...)
                        ## a@Bazz.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## a
                        movq 48(%r12), %r13
                        cmpq $0, %r13
			jne l21
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l21
l21:                    pushq %r13
                        ## obtain vtable for class Bazz
                        movq $Bazz..vtable, %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## g.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## g
                        movq 32(%r12), %r13
                        cmpq $0, %r13
			jne l22
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l22
l22:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Foo
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## e.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## e
                        movq 64(%r12), %r13
                        cmpq $0, %r13
			jne l23
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l23
l23:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Bar
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 72(%r12)
                        ## self[10] c initializer <- doh(...)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 80(%r12)
                        ## self[11] d initializer <- printh(...)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 88(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bazz..new
Bazz..new:              ## constructor for Bazz
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 2
                        movq $16, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $6, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $11, %r14
                        movq %r14, 0(%r12)
                        movq $6, %r14
                        movq %r14, 8(%r12)
                        movq $Bazz..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field h (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] holds field g (Foo)
                        movq $0, %r13
                        movq %r13, 32(%r12)
                        ## self[5] holds field i (Object)
                        movq $0, %r13
                        movq %r13, 40(%r12)
                        ## self[3] h initializer <- 1
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## self[4] g initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l24
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Bazz will jump to l25
                        ## case Razz will jump to l26
                        ## case Foo will jump to l27
                        ## case Bar will jump to l28
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l28
                        movq $11, %r14
                        cmpq %r14, %r13
			je l25
                        movq $0, %r14
                        cmpq %r14, %r13
			je l29
                        movq $12, %r14
                        cmpq %r14, %r13
			je l27
                        movq $13, %r14
                        cmpq %r14, %r13
			je l29
                        movq $1, %r14
                        cmpq %r14, %r13
			je l29
                        movq $14, %r14
                        cmpq %r14, %r13
			je l29
                        movq $15, %r14
                        cmpq %r14, %r13
			je l29
                        movq $16, %r14
                        cmpq %r14, %r13
			je l26
                        movq $3, %r14
                        cmpq %r14, %r13
			je l29
.globl l29
l29:                    ## case expression: error case
                        movq $string11, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l24
l24:                    ## case expression: void case
                        movq $string12, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l25
l25:                    ## fp[0] holds case n (Bazz)
                        ## new Foo
                        pushq %rbp
                        pushq %r12
                        movq $Foo..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l30
.globl l26
l26:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l30
.globl l27
l27:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l30
.globl l28
l28:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l30
.globl l30
l30:                    ## case expression ends
                        movq %r13, 32(%r12)
                        ## self[5] i initializer <- printh(...)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Bazz
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 40(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bool..new
Bool..new:              ## constructor for Bool
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $4, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $0, %r14
                        movq %r14, 0(%r12)
                        movq $4, %r14
                        movq %r14, 8(%r12)
                        movq $Bool..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field (raw content) (Int)
                        movq $0, %r13
                        movq %r13, 24(%r12)
                        ## self[3] (raw content) initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Foo..new
Foo..new:               ## constructor for Foo
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 2
                        movq $16, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $8, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $12, %r14
                        movq %r14, 0(%r12)
                        movq $8, %r14
                        movq %r14, 8(%r12)
                        movq $Foo..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field h (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] holds field g (Foo)
                        movq $0, %r13
                        movq %r13, 32(%r12)
                        ## self[5] holds field i (Object)
                        movq $0, %r13
                        movq %r13, 40(%r12)
                        ## self[6] holds field a (Razz)
                        movq $0, %r13
                        movq %r13, 48(%r12)
                        ## self[7] holds field b (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 56(%r12)
                        ## self[3] h initializer <- 1
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## self[4] g initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l31
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Bazz will jump to l32
                        ## case Razz will jump to l33
                        ## case Foo will jump to l34
                        ## case Bar will jump to l35
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l35
                        movq $11, %r14
                        cmpq %r14, %r13
			je l32
                        movq $0, %r14
                        cmpq %r14, %r13
			je l36
                        movq $12, %r14
                        cmpq %r14, %r13
			je l34
                        movq $13, %r14
                        cmpq %r14, %r13
			je l36
                        movq $1, %r14
                        cmpq %r14, %r13
			je l36
                        movq $14, %r14
                        cmpq %r14, %r13
			je l36
                        movq $15, %r14
                        cmpq %r14, %r13
			je l36
                        movq $16, %r14
                        cmpq %r14, %r13
			je l33
                        movq $3, %r14
                        cmpq %r14, %r13
			je l36
.globl l36
l36:                    ## case expression: error case
                        movq $string11, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l31
l31:                    ## case expression: void case
                        movq $string12, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l32
l32:                    ## fp[0] holds case n (Bazz)
                        ## new Foo
                        pushq %rbp
                        pushq %r12
                        movq $Foo..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l37
.globl l33
l33:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l37
.globl l34
l34:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l37
.globl l35
l35:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l37
.globl l37
l37:                    ## case expression ends
                        movq %r13, 32(%r12)
                        ## self[5] i initializer <- printh(...)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Foo
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 40(%r12)
                        ## self[6] a initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l38
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Razz will jump to l39
                        ## case Foo will jump to l40
                        ## case Bar will jump to l41
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l41
                        movq $11, %r14
                        cmpq %r14, %r13
			je l42
                        movq $0, %r14
                        cmpq %r14, %r13
			je l42
                        movq $12, %r14
                        cmpq %r14, %r13
			je l40
                        movq $13, %r14
                        cmpq %r14, %r13
			je l42
                        movq $1, %r14
                        cmpq %r14, %r13
			je l42
                        movq $14, %r14
                        cmpq %r14, %r13
			je l42
                        movq $15, %r14
                        cmpq %r14, %r13
			je l42
                        movq $16, %r14
                        cmpq %r14, %r13
			je l39
                        movq $3, %r14
                        cmpq %r14, %r13
			je l42
.globl l42
l42:                    ## case expression: error case
                        movq $string13, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l38
l38:                    ## case expression: void case
                        movq $string14, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l39
l39:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l43
.globl l40
l40:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l43
.globl l41
l41:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l43
.globl l43
l43:                    ## case expression ends
                        movq %r13, 48(%r12)
                        ## self[7] b initializer <- a.doh(...) + g.doh(...) + doh(...) + printh(...)
                        ## a.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## a
                        movq 48(%r12), %r13
                        cmpq $0, %r13
			jne l44
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l44
l44:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Razz
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## g.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## g
                        movq 32(%r12), %r13
                        cmpq $0, %r13
			jne l45
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l45
l45:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Foo
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Foo
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Foo
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 56(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..new
IO..new:                ## constructor for IO
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $3, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $13, %r14
                        movq %r14, 0(%r12)
                        movq $3, %r14
                        movq %r14, 8(%r12)
                        movq $IO..vtable, %r14
                        movq %r14, 16(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..new
Int..new:               ## constructor for Int
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $4, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $1, %r14
                        movq %r14, 0(%r12)
                        movq $4, %r14
                        movq %r14, 8(%r12)
                        movq $Int..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field (raw content) (Int)
                        movq $0, %r13
                        movq %r13, 24(%r12)
                        ## self[3] (raw content) initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..new
Main..new:              ## constructor for Main
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $7, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $14, %r14
                        movq %r14, 0(%r12)
                        movq $7, %r14
                        movq %r14, 8(%r12)
                        movq $Main..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field a (Bazz)
                        movq $0, %r13
                        movq %r13, 24(%r12)
                        ## self[4] holds field b (Foo)
                        movq $0, %r13
                        movq %r13, 32(%r12)
                        ## self[5] holds field c (Razz)
                        movq $0, %r13
                        movq %r13, 40(%r12)
                        ## self[6] holds field d (Bar)
                        movq $0, %r13
                        movq %r13, 48(%r12)
                        ## self[3] a initializer <- new Bazz
                        ## new Bazz
                        pushq %rbp
                        pushq %r12
                        movq $Bazz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] b initializer <- new Foo
                        ## new Foo
                        pushq %rbp
                        pushq %r12
                        movq $Foo..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 32(%r12)
                        ## self[5] c initializer <- new Razz
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 40(%r12)
                        ## self[6] d initializer <- new Bar
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 48(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object..new
Object..new:            ## constructor for Object
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $3, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $15, %r14
                        movq %r14, 0(%r12)
                        movq $3, %r14
                        movq %r14, 8(%r12)
                        movq $Object..vtable, %r14
                        movq %r14, 16(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Razz..new
Razz..new:              ## constructor for Razz
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 2
                        movq $16, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $10, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $16, %r14
                        movq %r14, 0(%r12)
                        movq $10, %r14
                        movq %r14, 8(%r12)
                        movq $Razz..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field h (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] holds field g (Foo)
                        movq $0, %r13
                        movq %r13, 32(%r12)
                        ## self[5] holds field i (Object)
                        movq $0, %r13
                        movq %r13, 40(%r12)
                        ## self[6] holds field a (Razz)
                        movq $0, %r13
                        movq %r13, 48(%r12)
                        ## self[7] holds field b (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 56(%r12)
                        ## self[8] holds field e (Bar)
                        movq $0, %r13
                        movq %r13, 64(%r12)
                        ## self[9] holds field f (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 72(%r12)
                        ## self[3] h initializer <- 1
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## self[4] g initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l46
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Bazz will jump to l47
                        ## case Razz will jump to l48
                        ## case Foo will jump to l49
                        ## case Bar will jump to l50
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l50
                        movq $11, %r14
                        cmpq %r14, %r13
			je l47
                        movq $0, %r14
                        cmpq %r14, %r13
			je l51
                        movq $12, %r14
                        cmpq %r14, %r13
			je l49
                        movq $13, %r14
                        cmpq %r14, %r13
			je l51
                        movq $1, %r14
                        cmpq %r14, %r13
			je l51
                        movq $14, %r14
                        cmpq %r14, %r13
			je l51
                        movq $15, %r14
                        cmpq %r14, %r13
			je l51
                        movq $16, %r14
                        cmpq %r14, %r13
			je l48
                        movq $3, %r14
                        cmpq %r14, %r13
			je l51
.globl l51
l51:                    ## case expression: error case
                        movq $string11, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l46
l46:                    ## case expression: void case
                        movq $string12, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l47
l47:                    ## fp[0] holds case n (Bazz)
                        ## new Foo
                        pushq %rbp
                        pushq %r12
                        movq $Foo..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l52
.globl l48
l48:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l52
.globl l49
l49:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l52
.globl l50
l50:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l52
.globl l52
l52:                    ## case expression ends
                        movq %r13, 32(%r12)
                        ## self[5] i initializer <- printh(...)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Razz
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq %r13, 40(%r12)
                        ## self[6] a initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l53
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Razz will jump to l54
                        ## case Foo will jump to l55
                        ## case Bar will jump to l56
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l56
                        movq $11, %r14
                        cmpq %r14, %r13
			je l57
                        movq $0, %r14
                        cmpq %r14, %r13
			je l57
                        movq $12, %r14
                        cmpq %r14, %r13
			je l55
                        movq $13, %r14
                        cmpq %r14, %r13
			je l57
                        movq $1, %r14
                        cmpq %r14, %r13
			je l57
                        movq $14, %r14
                        cmpq %r14, %r13
			je l57
                        movq $15, %r14
                        cmpq %r14, %r13
			je l57
                        movq $16, %r14
                        cmpq %r14, %r13
			je l54
                        movq $3, %r14
                        cmpq %r14, %r13
			je l57
.globl l57
l57:                    ## case expression: error case
                        movq $string13, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l53
l53:                    ## case expression: void case
                        movq $string14, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l54
l54:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l58
.globl l55
l55:                    ## fp[0] holds case n (Foo)
                        ## new Razz
                        pushq %rbp
                        pushq %r12
                        movq $Razz..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l58
.globl l56
l56:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l58
.globl l58
l58:                    ## case expression ends
                        movq %r13, 48(%r12)
                        ## self[7] b initializer <- a.doh(...) + g.doh(...) + doh(...) + printh(...)
                        ## a.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## a
                        movq 48(%r12), %r13
                        cmpq $0, %r13
			jne l59
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l59
l59:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Razz
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## g.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## g
                        movq 32(%r12), %r13
                        cmpq $0, %r13
			jne l60
                        movq $string15, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l60
l60:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Foo
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Razz
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Razz
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 56(%r12)
                        ## self[8] e initializer <- case self ...
                        ## case expression begins
                        movq %r12, %r13
                        cmpq $0, %r13
			je l61
                        movq %r13, 0(%rbp)
                        movq 0(%r13), %r13
                        ## case Razz will jump to l62
                        ## case Bar will jump to l63
                        ## case expression: compare type tags
                        movq $10, %r14
                        cmpq %r14, %r13
			je l63
                        movq $11, %r14
                        cmpq %r14, %r13
			je l64
                        movq $0, %r14
                        cmpq %r14, %r13
			je l64
                        movq $12, %r14
                        cmpq %r14, %r13
			je l64
                        movq $13, %r14
                        cmpq %r14, %r13
			je l64
                        movq $1, %r14
                        cmpq %r14, %r13
			je l64
                        movq $14, %r14
                        cmpq %r14, %r13
			je l64
                        movq $15, %r14
                        cmpq %r14, %r13
			je l64
                        movq $16, %r14
                        cmpq %r14, %r13
			je l62
                        movq $3, %r14
                        cmpq %r14, %r13
			je l64
.globl l64
l64:                    ## case expression: error case
                        movq $string16, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l61
l61:                    ## case expression: void case
                        movq $string17, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
                        ## case expression: branches
.globl l62
l62:                    ## fp[0] holds case n (Razz)
                        ## new Bar
                        pushq %rbp
                        pushq %r12
                        movq $Bar..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp l65
.globl l63
l63:                    ## fp[0] holds case n (Bar)
                        ## n
                        movq 0(%rbp), %r13
                        jmp l65
.globl l65
l65:                    ## case expression ends
                        movq %r13, 64(%r12)
                        ## self[9] f initializer <- a@Bazz.doh(...) + g.doh(...) + e.doh(...) + doh(...) + printh(...)
                        ## a@Bazz.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## a
                        movq 48(%r12), %r13
                        cmpq $0, %r13
			jne l66
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l66
l66:                    pushq %r13
                        ## obtain vtable for class Bazz
                        movq $Bazz..vtable, %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## g.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## g
                        movq 32(%r12), %r13
                        cmpq $0, %r13
			jne l67
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l67
l67:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Foo
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## e.doh(...)
                        pushq %r12
                        pushq %rbp
                        ## e
                        movq 64(%r12), %r13
                        cmpq $0, %r13
			jne l68
                        movq $string18, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l68
l68:                    pushq %r13
                        ## obtain vtable from object in r1 with static type Bar
                        movq 16(%r13), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## doh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Razz
                        movq 16(%r12), %r14
                        ## look up doh() at offset 10 in vtable
                        movq 80(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq %r13, 0(%rbp)
                        ## printh(...)
                        pushq %r12
                        pushq %rbp
                        pushq %r12
                        ## obtain vtable for self object of type Razz
                        movq 16(%r12), %r14
                        ## look up printh() at offset 9 in vtable
                        movq 72(%r14), %r14
                        call *%r14
                        addq $8, %rsp
                        popq %rbp
                        popq %r12
                        movq 24(%r13), %r13
                        movq 0(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, 0(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq 0(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 72(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String..new
String..new:            ## constructor for String
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $4, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $3, %r14
                        movq %r14, 0(%r12)
                        movq $4, %r14
                        movq %r14, 8(%r12)
                        movq $String..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field (raw content) (String)
                        movq $the.empty.string, %r13
                        movq %r13, 24(%r12)
                        ## self[3] (raw content) initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object.abort
Object.abort:           ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        movq $string19, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl Object.abort.end
Object.abort.end:       ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object.copy
Object.copy:            ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        movq 8(%r12), %r14
                        movq $8, %rsi
			movq %r14, %rdi
			call calloc
			movq %rax, %r13
                        pushq %r13
.globl l69
l69:                    cmpq $0, %r14
			je l70
                        movq 0(%r12), %r15
                        movq %r15, 0(%r13)
                        movq $8, %r15
                        addq %r15, %r12
                        addq %r15, %r13
                        movq $1, %r15
                        subq %r15, %r14
                        jmp l69
.globl l70
l70:                    ## done with Object.copy loop
                        popq %r13
.globl Object.copy.end
Object.copy.end:        ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object.type_name
Object.type_name:       ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        ## new String
                        pushq %rbp
                        pushq %r12
                        movq $String..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        ## obtain vtable for self object
                        movq 16(%r12), %r14
                        ## look up type name at offset 0 in vtable
                        movq 0(%r14), %r14
                        movq %r14, 24(%r13)
.globl Object.type_name.end
Object.type_name.end:   ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.in_int
IO.in_int:              ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r14
                        movl	$1, %esi
			movl $4096, %edi
			call calloc
			pushq %rax
			movq %rax, %rdi
			movq $4096, %rsi 
			movq stdin(%rip), %rdx
			call fgets 
			popq %rdi 
			movl $0, %eax
			pushq %rax
			movq %rsp, %rdx
			movq $percent.ld, %rsi
			call sscanf
			popq %rax
			movq $0, %rsi 
			cmpq $2147483647, %rax 
			cmovg %rsi, %rax
			cmpq $-2147483648, %rax 
			cmovl %rsi, %rax
			movq %rax, %r13
                        movq %r13, 24(%r14)
                        movq %r14, %r13
.globl IO.in_int.end
IO.in_int.end:          ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.in_string
IO.in_string:           ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        ## new String
                        pushq %rbp
                        pushq %r12
                        movq $String..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r14
                        call coolgetstr 
			movq %rax, %r13
                        movq %r13, 24(%r14)
                        movq %r14, %r13
.globl IO.in_string.end
IO.in_string.end:       ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.out_int
IO.out_int:             ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## fp[3] holds argument x (Int)
                        ## method body begins
                        movq 24(%rbp), %r14
                        movq 24(%r14), %r13
                        movq $percent.d, %rdi
		movl %r13d, %eax
		cdqe
		movq %rax, %rsi
			movl $0, %eax
			call printf
                        movq %r12, %r13
.globl IO.out_int.end
IO.out_int.end:         ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.out_string
IO.out_string:          ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## fp[3] holds argument x (String)
                        ## method body begins
                        movq 24(%rbp), %r14
                        movq 24(%r14), %r13
                        movq %r13, %rdi
			call cooloutstr
                        movq %r12, %r13
.globl IO.out_string.end
IO.out_string.end:      ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bazz.printh
Bazz.printh:            ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        ## out_int(...)
                        pushq %r12
                        pushq %rbp
                        ## h
                        movq 24(%r12), %r13
                        pushq %r13
                        pushq %r12
                        ## obtain vtable for self object of type Bar
                        movq 16(%r12), %r14
                        ## look up out_int() at offset 7 in vtable
                        movq 56(%r14), %r14
                        call *%r14
                        addq $16, %rsp
                        popq %rbp
                        popq %r12
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $0, %r14
                        movq %r14, 24(%r13)
.globl Bazz.printh.end
Bazz.printh.end:        ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Foo.doh
Foo.doh:                ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 3
                        movq $24, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## self[6] holds field a (Razz)
                        ## self[7] holds field b (Int)
                        ## self[8] holds field e (Bar)
                        ## self[9] holds field f (Int)
                        ## self[10] holds field c (Int)
                        ## self[11] holds field d (Object)
                        ## method body begins
                        ## fp[0] holds local i (Int)
                        ## h
                        movq 24(%r12), %r13
                        movq %r13, 0(%rbp)
                        ## h
                        movq 24(%r12), %r13
                        movq 24(%r13), %r13
                        movq %r13, -8(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $2, %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq -8(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, -8(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq -8(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## i
                        movq 0(%rbp), %r13
.globl Foo.doh.end
Foo.doh.end:            ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bazz.doh
Bazz.doh:               ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 3
                        movq $24, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field h (Int)
                        ## self[4] holds field g (Foo)
                        ## self[5] holds field i (Object)
                        ## method body begins
                        ## fp[0] holds local i (Int)
                        ## h
                        movq 24(%r12), %r13
                        movq %r13, 0(%rbp)
                        ## h
                        movq 24(%r12), %r13
                        movq 24(%r13), %r13
                        movq %r13, -8(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        movq 24(%r13), %r13
                        movq -8(%rbp), %r14
                        addq %r14, %r13
                        movq %r13, -8(%rbp)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq -8(%rbp), %r14
                        movq %r14, 24(%r13)
                        movq %r13, 24(%r12)
                        ## i
                        movq 0(%rbp), %r13
.globl Bazz.doh.end
Bazz.doh.end:           ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main.main
Main.main:              ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## self[3] holds field a (Bazz)
                        ## self[4] holds field b (Foo)
                        ## self[5] holds field c (Razz)
                        ## self[6] holds field d (Bar)
                        ## method body begins
                        ## new String
                        pushq %rbp
                        pushq %r12
                        movq $String..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        ## string20 holds "do nothing"
                        movq $string20, %r14
                        movq %r14, 24(%r13)
.globl Main.main.end
Main.main.end:          ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.concat
String.concat:          ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## fp[3] holds argument s (String)
                        ## method body begins
                        ## new String
                        pushq %rbp
                        pushq %r12
                        movq $String..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r15
                        movq 24(%rbp), %r14
                        movq 24(%r14), %r14
                        movq 24(%r12), %r13
                        movq %r13, %rdi
			movq %r14, %rsi
			call coolstrcat
			movq %rax, %r13
                        movq %r13, 24(%r15)
                        movq %r15, %r13
.globl String.concat.end
String.concat.end:      ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.length
String.length:          ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## method body begins
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r14
                        movq 24(%r12), %r13
                        movq %r13, %rdi
			movl $0, %eax
			call coolstrlen
			movq %rax, %r13
                        movq %r13, 24(%r14)
                        movq %r14, %r13
.globl String.length.end
String.length.end:      ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.substr
String.substr:          ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## fp[4] holds argument i (Int)
                        ## fp[3] holds argument l (Int)
                        ## method body begins
                        ## new String
                        pushq %rbp
                        pushq %r12
                        movq $String..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r15
                        movq 24(%rbp), %r14
                        movq 24(%r14), %r14
                        movq 32(%rbp), %r13
                        movq 24(%r13), %r13
                        movq 24(%r12), %r12
                        movq %r12, %rdi
			movq %r13, %rsi
			movq %r14, %rdx
			call coolsubstr
			movq %rax, %r13
                        cmpq $0, %r13
			jne l71
                        movq $string21, %r13
                        movq %r13, %rdi
			call cooloutstr
                        movl $0, %edi
			call exit
.globl l71
l71:                    movq %r13, 24(%r15)
                        movq %r15, %r13
.globl String.substr.end
String.substr.end:      ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ## global string constants
.globl the.empty.string
the.empty.string:       # ""
.byte 0

.globl percent.d
percent.d:              # "%ld"
.byte  37 # '%'
.byte 108 # 'l'
.byte 100 # 'd'
.byte 0

.globl percent.ld
percent.ld:             # " %ld"
.byte  32 # ' '
.byte  37 # '%'
.byte 108 # 'l'
.byte 100 # 'd'
.byte 0

.globl string1
string1:                # "Bar"
.byte  66 # 'B'
.byte  97 # 'a'
.byte 114 # 'r'
.byte 0

.globl string2
string2:                # "Bazz"
.byte  66 # 'B'
.byte  97 # 'a'
.byte 122 # 'z'
.byte 122 # 'z'
.byte 0

.globl string3
string3:                # "Bool"
.byte  66 # 'B'
.byte 111 # 'o'
.byte 111 # 'o'
.byte 108 # 'l'
.byte 0

.globl string4
string4:                # "Foo"
.byte  70 # 'F'
.byte 111 # 'o'
.byte 111 # 'o'
.byte 0

.globl string5
string5:                # "IO"
.byte  73 # 'I'
.byte  79 # 'O'
.byte 0

.globl string6
string6:                # "Int"
.byte  73 # 'I'
.byte 110 # 'n'
.byte 116 # 't'
.byte 0

.globl string7
string7:                # "Main"
.byte  77 # 'M'
.byte  97 # 'a'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 0

.globl string8
string8:                # "Object"
.byte  79 # 'O'
.byte  98 # 'b'
.byte 106 # 'j'
.byte 101 # 'e'
.byte  99 # 'c'
.byte 116 # 't'
.byte 0

.globl string9
string9:                # "Razz"
.byte  82 # 'R'
.byte  97 # 'a'
.byte 122 # 'z'
.byte 122 # 'z'
.byte 0

.globl string10
string10:               # "String"
.byte  83 # 'S'
.byte 116 # 't'
.byte 114 # 'r'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte 0

.globl string11
string11:               # "ERROR: 39: Exception: case without matching branch\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  51 # '3'
.byte  57 # '9'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 119 # 'w'
.byte 105 # 'i'
.byte 116 # 't'
.byte 104 # 'h'
.byte 111 # 'o'
.byte 117 # 'u'
.byte 116 # 't'
.byte  32 # ' '
.byte 109 # 'm'
.byte  97 # 'a'
.byte 116 # 't'
.byte  99 # 'c'
.byte 104 # 'h'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte  32 # ' '
.byte  98 # 'b'
.byte 114 # 'r'
.byte  97 # 'a'
.byte 110 # 'n'
.byte  99 # 'c'
.byte 104 # 'h'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string12
string12:               # "ERROR: 39: Exception: case on void\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  51 # '3'
.byte  57 # '9'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 111 # 'o'
.byte 110 # 'n'
.byte  32 # ' '
.byte 118 # 'v'
.byte 111 # 'o'
.byte 105 # 'i'
.byte 100 # 'd'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string13
string13:               # "ERROR: 4: Exception: case without matching branch\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  52 # '4'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 119 # 'w'
.byte 105 # 'i'
.byte 116 # 't'
.byte 104 # 'h'
.byte 111 # 'o'
.byte 117 # 'u'
.byte 116 # 't'
.byte  32 # ' '
.byte 109 # 'm'
.byte  97 # 'a'
.byte 116 # 't'
.byte  99 # 'c'
.byte 104 # 'h'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte  32 # ' '
.byte  98 # 'b'
.byte 114 # 'r'
.byte  97 # 'a'
.byte 110 # 'n'
.byte  99 # 'c'
.byte 104 # 'h'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string14
string14:               # "ERROR: 4: Exception: case on void\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  52 # '4'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 111 # 'o'
.byte 110 # 'n'
.byte  32 # ' '
.byte 118 # 'v'
.byte 111 # 'o'
.byte 105 # 'i'
.byte 100 # 'd'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string15
string15:               # "ERROR: 10: Exception: dispatch on void\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  49 # '1'
.byte  48 # '0'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte 100 # 'd'
.byte 105 # 'i'
.byte 115 # 's'
.byte 112 # 'p'
.byte  97 # 'a'
.byte 116 # 't'
.byte  99 # 'c'
.byte 104 # 'h'
.byte  32 # ' '
.byte 111 # 'o'
.byte 110 # 'n'
.byte  32 # ' '
.byte 118 # 'v'
.byte 111 # 'o'
.byte 105 # 'i'
.byte 100 # 'd'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string16
string16:               # "ERROR: 26: Exception: case without matching branch\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  50 # '2'
.byte  54 # '6'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 119 # 'w'
.byte 105 # 'i'
.byte 116 # 't'
.byte 104 # 'h'
.byte 111 # 'o'
.byte 117 # 'u'
.byte 116 # 't'
.byte  32 # ' '
.byte 109 # 'm'
.byte  97 # 'a'
.byte 116 # 't'
.byte  99 # 'c'
.byte 104 # 'h'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte  32 # ' '
.byte  98 # 'b'
.byte 114 # 'r'
.byte  97 # 'a'
.byte 110 # 'n'
.byte  99 # 'c'
.byte 104 # 'h'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string17
string17:               # "ERROR: 26: Exception: case on void\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  50 # '2'
.byte  54 # '6'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  99 # 'c'
.byte  97 # 'a'
.byte 115 # 's'
.byte 101 # 'e'
.byte  32 # ' '
.byte 111 # 'o'
.byte 110 # 'n'
.byte  32 # ' '
.byte 118 # 'v'
.byte 111 # 'o'
.byte 105 # 'i'
.byte 100 # 'd'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string18
string18:               # "ERROR: 31: Exception: dispatch on void\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  51 # '3'
.byte  49 # '1'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte 100 # 'd'
.byte 105 # 'i'
.byte 115 # 's'
.byte 112 # 'p'
.byte  97 # 'a'
.byte 116 # 't'
.byte  99 # 'c'
.byte 104 # 'h'
.byte  32 # ' '
.byte 111 # 'o'
.byte 110 # 'n'
.byte  32 # ' '
.byte 118 # 'v'
.byte 111 # 'o'
.byte 105 # 'i'
.byte 100 # 'd'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string19
string19:               # "abort\\n"
.byte  97 # 'a'
.byte  98 # 'b'
.byte 111 # 'o'
.byte 114 # 'r'
.byte 116 # 't'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

.globl string20
string20:               # "do nothing"
.byte 100 # 'd'
.byte 111 # 'o'
.byte  32 # ' '
.byte 110 # 'n'
.byte 111 # 'o'
.byte 116 # 't'
.byte 104 # 'h'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte 0

.globl string21
string21:               # "ERROR: 0: Exception: String.substr out of range\\n"
.byte  69 # 'E'
.byte  82 # 'R'
.byte  82 # 'R'
.byte  79 # 'O'
.byte  82 # 'R'
.byte  58 # ':'
.byte  32 # ' '
.byte  48 # '0'
.byte  58 # ':'
.byte  32 # ' '
.byte  69 # 'E'
.byte 120 # 'x'
.byte  99 # 'c'
.byte 101 # 'e'
.byte 112 # 'p'
.byte 116 # 't'
.byte 105 # 'i'
.byte 111 # 'o'
.byte 110 # 'n'
.byte  58 # ':'
.byte  32 # ' '
.byte  83 # 'S'
.byte 116 # 't'
.byte 114 # 'r'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 103 # 'g'
.byte  46 # '.'
.byte 115 # 's'
.byte 117 # 'u'
.byte  98 # 'b'
.byte 115 # 's'
.byte 116 # 't'
.byte 114 # 'r'
.byte  32 # ' '
.byte 111 # 'o'
.byte 117 # 'u'
.byte 116 # 't'
.byte  32 # ' '
.byte 111 # 'o'
.byte 102 # 'f'
.byte  32 # ' '
.byte 114 # 'r'
.byte  97 # 'a'
.byte 110 # 'n'
.byte 103 # 'g'
.byte 101 # 'e'
.byte  92 # '\\'
.byte 110 # 'n'
.byte 0

                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl eq_handler
eq_handler:             ## helper function for =
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je eq_true
                        movq $0, %r15
                        cmpq %r15, %r13
			je eq_false
                        cmpq %r15, %r14
			je eq_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je eq_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je eq_int
                        movq $6, %r14
                        cmpq %r14, %r13
			je eq_string
                        ## otherwise, use pointer comparison
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je eq_true
.globl eq_false
eq_false:               ## not equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp eq_end
.globl eq_true
eq_true:                ## equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp eq_end
.globl eq_bool
eq_bool:                ## two Bools
.globl eq_int
eq_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpq %r14, %r13
			je eq_true
                        jmp eq_false
.globl eq_string
eq_string:              ## two Strings
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        
  movq %r13, %rdi
  movq %r14, %rsi
  call strcmp 
  cmp $0, %eax
  je eq_true
                        jmp eq_false
.globl eq_end
eq_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl le_handler
le_handler:             ## helper function for <=
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je le_true
                        movq $0, %r15
                        cmpq %r15, %r13
			je le_false
                        cmpq %r15, %r14
			je le_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je le_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je le_int
                        movq $6, %r14
                        cmpq %r14, %r13
			je le_string
                        ## for non-primitives, equality is our only hope
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je le_true
.globl le_false
le_false:               ## not less-than-or-equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp le_end
.globl le_true
le_true:                ## less-than-or-equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp le_end
.globl le_bool
le_bool:                ## two Bools
.globl le_int
le_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpl %r14d, %r13d
			jle le_true
                        jmp le_false
.globl le_string
le_string:              ## two Strings
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        
  movq %r13, %rdi
  movq %r14, %rsi
  call strcmp 
  cmp $0, %eax
  jle le_true
                        jmp le_false
.globl le_end
le_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl lt_handler
lt_handler:             ## helper function for <
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq $0, %r15
                        cmpq %r15, %r13
			je lt_false
                        cmpq %r15, %r14
			je lt_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je lt_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je lt_int
                        movq $6, %r14
                        cmpq %r14, %r13
			je lt_string
                        ## for non-primitives, < is always false
.globl lt_false
lt_false:               ## not less than
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp lt_end
.globl lt_true
lt_true:                ## less than
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp lt_end
.globl lt_bool
lt_bool:                ## two Bools
.globl lt_int
lt_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpl %r14d, %r13d
			jl lt_true
                        jmp lt_false
.globl lt_string
lt_string:              ## two Strings
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        
  movq %r13, %rdi
  movq %r14, %rsi
  call strcmp 
  cmp $0, %eax
  jl lt_true
                        jmp lt_false
.globl lt_end
lt_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl start
start:                  ## program begins here
                        .globl main
			.type main, @function
main:
                        movq $Main..new, %r14
                        pushq %rbp
                        call *%r14
                        pushq %rbp
                        pushq %r13
                        movq $Main.main, %r14
                        call *%r14
                        movl $0, %edi
			call exit
                        
.globl cooloutstr
	.type	cooloutstr, @function
cooloutstr:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L2
.L5:
	movl	-4(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L3
	movl	-4(%rbp), %eax
	cltq
	addq	$1, %rax
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	cmpb	$110, %al
	jne	.L3
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$10, %edi
	call	fputc
	addl	$2, -4(%rbp)
	jmp	.L2
.L3:
	movl	-4(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L4
	movl	-4(%rbp), %eax
	cltq
	addq	$1, %rax
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	cmpb	$116, %al
	jne	.L4
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$9, %edi
	call	fputc
	addl	$2, -4(%rbp)
	jmp	.L2
.L4:
	movq	stdout(%rip), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	movsbl	%al, %eax
	movq	%rdx, %rsi
	movl	%eax, %edi
	call	fputc
	addl	$1, -4(%rbp)
.L2:
	movl	-4(%rbp), %eax
	cltq
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L5
	movq	stdout(%rip), %rax
	movq	%rax, %rdi
	call	fflush
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	cooloutstr, .-cooloutstr
.globl coolstrlen
	.type	coolstrlen, @function
coolstrlen:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -24(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L7
.L8:
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -4(%rbp)
.L7:
	movl	-4(%rbp), %eax
	mov	%eax, %eax
	addq	-24(%rbp), %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L8
	movl	-4(%rbp), %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	coolstrlen, .-coolstrlen
	.section	.rodata
.LC0:
	.string	"%s%s"
	.text
.globl coolstrcat
	.type	coolstrcat, @function
coolstrcat:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	cmpq	$0, -40(%rbp)
	jne	.L10
	.cfi_offset 3, -24
	movq	-48(%rbp), %rax
	jmp	.L11
.L10:
	cmpq	$0, -48(%rbp)
	jne	.L12
	movq	-40(%rbp), %rax
	jmp	.L11
.L12:
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, %ebx
	movq	-48(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	leal	(%rbx,%rax), %eax
	addl	$1, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	cltq
	movl	$1, %esi
	movq	%rax, %rdi
	call	calloc
	movq	%rax, -32(%rbp)
	movl	$.LC0, %edx
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-48(%rbp), %rsi
	movq	-40(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	%rsi, %r8
	movq	%rbx, %rsi
	movq	%rax, %rdi
	movl	$0, %eax
	call	snprintf
	movq	-32(%rbp), %rax
.L11:
	addq	$40, %rsp
	popq	%rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	coolstrcat, .-coolstrcat
	.section	.rodata
.LC1:
	.string	""
	.text
.globl coolgetstr
	.type	coolgetstr, @function
coolgetstr:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	$1, %esi
	movl	$40960, %edi
	call	calloc
	movq	%rax, -16(%rbp)
	movl	$0, -4(%rbp)
.L20:
	movq	stdin(%rip), %rax
	movq	%rax, %rdi
	call	fgetc
	movl	%eax, -20(%rbp)
	cmpl	$-1, -20(%rbp)
	je	.L14
	cmpl	$10, -20(%rbp)
	jne	.L15
.L14:
	cmpl	$0, -4(%rbp)
	je	.L16
	movl	$.LC1, %eax
	jmp	.L17
.L16:
	movq	-16(%rbp), %rax
	jmp	.L17
.L15:
	cmpl	$0, -20(%rbp)
	jne	.L18
	movl	$1, -4(%rbp)
	jmp	.L20
.L18:
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	mov	%eax, %eax
	addq	-16(%rbp), %rax
	movl	-20(%rbp), %edx
	movb	%dl, (%rax)
	jmp	.L20
.L17:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	coolgetstr, .-coolgetstr
.globl coolsubstr
	.type	coolsubstr, @function
coolsubstr:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, -4(%rbp)
	cmpq	$0, -32(%rbp)
	js	.L22
	cmpq	$0, -40(%rbp)
	js	.L22
	movq	-40(%rbp), %rax
	movq	-32(%rbp), %rdx
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	jle	.L23
.L22:
	movl	$0, %eax
	jmp	.L24
.L23:
	movq	-40(%rbp), %rdx
	movq	-32(%rbp), %rax
	addq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	strndup
.L24:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	coolsubstr, .-coolsubstr

