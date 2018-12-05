open MinCaml

val gen_mj_fundef : [< `Meta_method | `Meta_tracing ] -> Asm.fundef -> Asm.fundef
val gen_mj : [< `Meta_method | `Meta_tracing ] -> Asm.prog -> Asm.prog
