comment start
label Main_main_0
t0 <- int 34
x <- t0
t1 <- int 18
y <- t1
t3 <- int 2
t4 <- * y t3
t5 <- < x t4
t6 <- not t5
bt t6 Main_main_2
label Main_main_1
t7 <- string 
x greater than\n
t8 <- call out_string t7
t2 <- t8
jmp Main_main_3
label Main_main_2
t9 <- string 
y greater than\n
t10 <- call out_string t9
t2 <- t10
label Main_main_3
return t2
