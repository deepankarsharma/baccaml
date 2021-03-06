open Asm

let (|*|) e (n1, n2) = match e with
  | IfEq _ -> n1 = n2
  | IfLE _ -> n1 <= n2
  | IfGE _ -> n1 >= n2
  | _ -> assert false

let (|%|) e (id_t, id_or_imm, t1, t2) = match e with
  | IfEq _ -> IfEq (id_t, id_or_imm, t1, t2)
  | IfLE _ -> IfLE (id_t, id_or_imm, t1, t2)
  | IfGE _ -> IfGE (id_t, id_or_imm, t1, t2)
  | _ -> assert false

let (|%%|) e (t1, t2) = match e with
  | IfEq (id_t, id_or_imm, _, _) -> IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, _, _) -> IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, _, _) -> IfGE (id_t, id_or_imm, t1, t2)
  | _ -> assert false
