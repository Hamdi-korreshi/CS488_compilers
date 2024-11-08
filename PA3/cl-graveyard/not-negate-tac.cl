comment start
label Main_main_0
t$3 <- y
t$5 <- int 3
t$4 <- ~ t$5
t$2 <- < t$3 t$4
t$1 <- not t$2
t$8 <- not t$1
bt t$8 Main_main_2
bt t$1 Main_main_1
comment then branch
label Main_main_1
t$6 <- string
A
jmp Main_main_4
comment else branch
label Main_main_2
t$7 <- string
B
jmp Main_main_6
comment if-join
label Main_main_3
return t$0
comment fcall-pre to out_string
label Main_main_4
t$0 <- call out_string t$6
jmp Main_main_3
comment fcall-pre to out_string
label Main_main_6
t$0 <- call out_string t$7
jmp Main_main_3
