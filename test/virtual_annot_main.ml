let code = Array.make 40 0 in
let stack = Array.make 10000 0 in
(* for meta tracing *)
(* 8 1 4 2 3 5 11 4 1 14 26 8 1 4 1 1 6 0 8 2 4 2 1 6 0 0 7 1 4 10 6 0 9 *)
code.(0) <- 8;
code.(1) <- 1;
code.(2) <- 4;
code.(3) <- 2;
code.(4) <- 3;
code.(5) <- 5;
code.(6) <- 11;
code.(7) <- 4;
code.(8) <- 1;
code.(9) <- 14;
code.(10) <- 26;
code.(11) <- 8;
code.(12) <- 1;
code.(13) <- 4;
code.(14) <- 1;
code.(15) <- 1;
code.(16) <- 6;
code.(17) <- 0;
code.(18) <- 8;
code.(19) <- 2;
code.(20) <- 4;
code.(21) <- 2;
code.(22) <- 1;
code.(23) <- 6;
code.(24) <- 0;
code.(25) <- 0;
code.(26) <- 7;
code.(27) <- 1;
code.(28) <- 4;
code.(29) <- 28;
code.(30) <- 6;
code.(31) <- 0;
code.(32) <- 9;
print_int (interp stack 0 code 28)

(* for meta method *)
(* code.(0) <- 8;
 * code.(1) <- 0;
 * code.(2) <- 4;
 * code.(3) <- 2;
 * code.(4) <- 3;
 * code.(5) <- 5;
 * code.(6) <- 11;
 * code.(7) <- 4;
 * code.(8) <- 1;
 * code.(9) <- 14;
 * code.(10) <- 26;
 * code.(11) <- 8;
 * code.(12) <- 0;
 * code.(13) <- 4;
 * code.(14) <- 1;
 * code.(15) <- 1;
 * code.(16) <- 6;
 * code.(17) <- 0;
 * code.(18) <- 8;
 * code.(19) <- 1;
 * code.(20) <- 4;
 * code.(21) <- 2;
 * code.(22) <- 1;
 * code.(23) <- 6;
 * code.(24) <- 0;
 * code.(25) <- 0;
 * code.(26) <- 7;
 * code.(27) <- 4;
 * code.(28) <- 28;
 * code.(29) <- 6;
 * code.(30) <- 0;
 * code.(31) <- 9;
 * (* 8 0 4 2 3 5 11 4 1 14 26 8 0 4 1 1 6 0 8 1 4 2 1 6 0 0 7 4 10 6 0 9 *)
 * print_int (interp stack 0 code 27) *)
