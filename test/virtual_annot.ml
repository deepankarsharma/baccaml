(* let print_array f arr =
 *   print_string "[|";
 *   Array.iter
 *     (fun a -> f a; print_string "; ")
 *     arr;
 *   print_string "|] " in
 *
 * let loop_start _ = () in
 * let loop_end _ = () in
 * let jit_dispatch _ _ _ _ = () in *)

let rec loop stack old_base new_base ret n i =
  if n = 1 then (stack.(old_base + n) <- ret; old_base + n + 1)
  else (stack.(old_base + i) <- stack.(new_base + i);
        loop stack old_base new_base ret n (i + 1))
in

let rec frame_reset stack sp o l n =
  let ret = stack.(sp-n-l-1) in
  let old_base = sp - n - l - o - 1 in
  let new_base = sp - n in
  loop stack old_base new_base ret n 0
in

let rec min_caml_interp stack sp bytecode pc =
  jit_dispatch (pc=0) stack sp bytecode;
  let instr = bytecode.(pc) in
  (* Printf.printf "is: %d\tsp: %d\tpc: %d\t" instr sp pc;
   * print_array print_int stack; print_newline (); *)
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    min_caml_interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    min_caml_interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    stack.(sp - 2) <- n;
    min_caml_interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    min_caml_interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0
    then min_caml_interp stack (sp - 1) bytecode addr
    else min_caml_interp stack (sp - 1) bytecode (pc + 2)
  else if instr = 6 then        (* CALL *)
    if is_mj () then
      let addr = bytecode.(pc + 1) in
      let r = min_caml_interp stack sp bytecode (bytecode.(pc + 1)) in
      stack.(sp - 1) <- r;
      min_caml_interp stack sp bytecode (pc + 2)
    else
      (stack.(sp) <- pc + 2;
       min_caml_interp stack (sp + 1) bytecode (bytecode.(pc + 1)))
  else if instr = 7 then        (* RET *)
    if is_mj () then
      stack.(sp - 1)
    else
     (let n = bytecode.(pc + 1) in
     let v = stack.(sp - 1) in   (* sp: sp - 1 *)
     let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
     stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
     min_caml_interp stack (sp - n - 1) bytecode pc2)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    min_caml_interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 10 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    min_caml_interp stack (sp - 2) bytecode (pc + 1)
  else if instr = 11 then       (* FRAME_RESET *)
    let o = bytecode.(pc + 1) in
    let l = bytecode.(pc + 2) in
    let n = bytecode.(pc + 3) in
    let sp2 = frame_reset stack sp o l n in
    min_caml_interp stack sp2 bytecode (pc + 4)
  else if instr = 12 then       (* LOOP_S *)
    (loop_start ();
     min_caml_min_caml_interp stack sp bytecode (pc + 1))
  else if instr = 13 then       (* LOOP_E *)
    (loop_end ();
     min_caml_interp stack sp bytecode (pc + 1))
  else if instr = 14 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    min_caml_interp stack sp bytecode addr
  else
    -1000
in
print_int (min_caml_interp (Array.create 0 0) 0 (Array.create 0 0) 0)
