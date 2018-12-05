open MinCaml
open Asm
open Emit
open Jit_config

let replace input output =
  Str.global_replace (Str.regexp_string input) output

let rec replace_in_list slst output =
  match slst with
  | [] -> []
  | hd :: tl -> (replace hd output) :: replace_in_list tl output

let write_string_of_list oc sl =
  List.iter (fun s -> Printf.fprintf oc s) sl

let find_interp_name fundefs =
  List.find begin fun { name = Id.L (x)} ->
    String.split_on_char '.' x
    |> List.hd
    |> fun s -> (String.equal "min_caml_interp" s) || (String.equal "min_caml_interpret" s)
  end fundefs |> fun { name = Id.L (x) } -> x

(* 命令列のアセンブリ生成 as String *)
let rec create_asm (dest, t) =
  let buf = Buffer.create 10000 in
  let rec go = function
    | dest, Ans (exp) ->
      create_asm' (dest, exp) |> Buffer.add_string buf
    | dest, Let ((x, t), exp, e) ->
      create_asm' (NonTail(x), exp) |> Buffer.add_string buf;
      go (dest, e)
  in go (dest, t);
  Buffer.contents buf

(* 各命令のアセンブリ生成 as String *)
and create_asm' = function
  | NonTail (_), Nop -> ""
  | NonTail (x), Set (i) -> Printf.sprintf "\tmovl\t$%d, %s\n" i x
  | NonTail (x), SetL (Id.L (y)) -> Printf.sprintf "\tmovl\t$%s, %s\n" y x
  | NonTail (x), Mov (y) ->
    let buf = Buffer.create 10 in
    if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), Neg (y) ->
    let buf = Buffer.create 100 in
    if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> Buffer.add_string buf;
    Printf.sprintf "\tnegl\t%s\n" x |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail (x), Add (y, z') ->
    if V(x) = z' then
      Printf.sprintf "\taddl\t%s, %s\n" y x
    else begin
      let buf = Buffer.create 100 in
      if x <> y then
        Printf.sprintf "\tmovl\t%s, %s\n" y x
        |> Buffer.add_string buf;
      Printf.sprintf "\taddl\t%s, %s\n" (pp_id_or_imm z') x
      |> Buffer.add_string buf;
      Buffer.contents buf
    end
  | NonTail (x), Sub (y, z') ->
    if V (x) = z' then
      let buf = Buffer.create 100 in
      (Printf.sprintf "\tsubl\t%s, %s\n" y x)
      |> Buffer.add_string buf;
      (Printf.sprintf "\tnegl\t%s\n" x)
      |> Buffer.add_string buf;
      Buffer.contents buf
    else begin
      let buf = Buffer.create 100 in
      if x <> y then
        Printf.sprintf "\tmovl\t%s, %s\n" y x
        |> Buffer.add_string buf;
      Printf.sprintf "\tsubl\t%s, %s\n" (pp_id_or_imm z') x
      |> Buffer.add_string buf;
      Buffer.contents buf
    end
  | NonTail (x), Ld (y, V (z), i) ->
    let buf = Buffer.create 20 in
    if y = zero then
      Printf.sprintf "\tmovl\t%d(%%esp), %s\n" 4 x
      |> Buffer.add_string buf;
    Printf.sprintf "\tmovl\t(%s,%s,%d), %s\n" y z i x
    |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail (x), Ld (y, C (j), i) ->
    let buf = Buffer.create 20 in
    if y = zero then
      Printf.sprintf "\tmovl\t%d(%%esp), %s\n" 4 x
      |> Buffer.add_string buf;
    Printf.sprintf "\tmovl\t%d(%s), %s\n" (j * i) y x
    |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail (_), St (x, y, V (z), i) ->
    let buf = Buffer.create 20 in
    if y = zero then
      Printf.sprintf "\tmovl\t%d(%%esp), %s\n" 4 x
      |> Buffer.add_string buf;
    Printf.sprintf "\tmovl\t%s, (%s,%s,%d)\n" x y z i
  | NonTail (_), St (x, y, C (j), i) ->
    let buf = Buffer.create 20 in
    if y = zero then
      Printf.sprintf "\tmovl\t%d(%%esp), %s\n" 4 x
      |> Buffer.add_string buf;
    Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (j * i) y
  | NonTail (x), FMovD (y) ->
    let buf = Buffer.create 10 in
    if x <> y then
      Printf.sprintf "\tmovsd\t%s, %s\n" y x
      |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), FNegD(y) ->
    let buf = Buffer.create 100 in
    if x <> y then
      Printf.sprintf "\tmovsd\t%s, %s\n" y x
      |> Buffer.add_string buf;
    Printf.sprintf "\txorpd\tmin_caml_fnegd, %s\n" x
    |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), FAddD(y, z) ->
    let buf = Buffer.create 100 in
    if x = z then
      Printf.sprintf "\taddsd\t%s, %s\n" y x
      |> Buffer.add_string buf
    else begin
      if x <> y then
        Printf.sprintf "\tmovsd\t%s, %s\n" y x
        |> Buffer.add_string buf;
    end;
    Printf.sprintf "\taddsd\t%s, %s\n" z x
    |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), FSubD(y, z) ->
    let buf = Buffer.create 100 in
    if x = z then
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp |> Buffer.add_string buf;
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> Buffer.add_string buf;
      Printf.sprintf "\tsubsd\t%d(%s), %s\n" ss reg_sp x |> Buffer.add_string buf;
    else begin
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> Buffer.add_string buf;
    end;
    Printf.sprintf "\tsubsd\t%s, %s\n" z x |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), FMulD(y, z) ->
    let buf = Buffer.create 100 in
    if x = z then
      Printf.sprintf "\tmulsd\t%s, %s\n" y x |> Buffer.add_string buf
    else begin
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> Buffer.add_string buf
    end;
    Printf.sprintf "\tmulsd\t%s, %s\n" z x |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), FDivD(y, z) ->
    let buf = Buffer.create 100 in
    if x = z then
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp |> Buffer.add_string buf;
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> Buffer.add_string buf;
      Printf.sprintf "\tdivsd\t%d(%s), %s\n" ss reg_sp x |> Buffer.add_string buf;
    else begin
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> Buffer.add_string buf;
    end;
    Printf.sprintf "\tdivsd\t%s, %s\n" z x |> Buffer.add_string buf;
    Buffer.contents buf
  | NonTail(x), LdDF(y, V(z), i) -> Printf.sprintf "\tmovsd\t(%s,%s,%d), %s\n" y z i x
  | NonTail(x), LdDF(y, C(j), i) -> Printf.sprintf "\tmovsd\t%d(%s), %s\n" (j * i) y x
  | NonTail(_), StDF(x, y, V(z), i) -> Printf.sprintf "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Comment(s) -> Printf.sprintf "\t# %s\n" s
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ""
  | NonTail(x), Restore(y) when List.mem x allregs ->
    Printf.sprintf "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.sprintf "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@ create_asm' (NonTail (Id.gentmp Type.Unit), exp);
    Buffer.add_string buf @@ Printf.sprintf "\tret\n";
    Buffer.contents buf
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@  create_asm' (NonTail (regs.(0)), exp);
    Buffer.add_string buf @@ Printf.sprintf "\tret\n";
    Buffer.contents buf
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@ create_asm' (NonTail (fregs.(0)), exp);
    Buffer.add_string buf @@ Printf.sprintf "\tret\n";
    Buffer.contents buf
  | Tail, (Restore (x) as exp) ->
    (match locate x with
     | [i] -> create_asm' (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> create_asm' (NonTail(fregs.(0)), exp)
     | _ -> assert false) ^
    Printf.sprintf "\tret\n"
  | Tail, IfEq(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "je" "jne"
  | Tail, IfLE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "jle" "jg"
  | Tail, IfGE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "jge" "jl"
  | Tail, IfFEq(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_tail_if e1 e2 "je" "jne"
  | Tail, IfFLE(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_tail_if e1 e2 "jbe" "ja"
  | NonTail(z), IfEq(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfLE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jle" "jg"
  | NonTail(z), IfGE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jge" "jl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jbe" "ja"
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@ (create_asm'_args [(x, reg_cl)] ys zs)
    |> fun _ ->
    Buffer.add_string buf @@ Printf.sprintf "\tjmp\t*(%s)\n" reg_cl
    |> fun _ -> Buffer.contents buf
  | Tail, CallDir(Id.L (x), ys, zs) -> (* 末尾呼び出し *)
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@ create_asm'_args [] ys zs;
    if List.for_all (fun c -> String.contains x c) (Stringext.to_list "guard_failure") then
      Buffer.add_string buf @@ Printf.sprintf "\tjmp\t%s\n" "min_caml_mid_layer"
    else
      Buffer.add_string buf @@ Printf.sprintf "\tjmp\t%s\n" x;
    Buffer.contents buf
  | NonTail(a), CallCls(x, ys, zs) ->
    Buffer.create 100
    |> fun buf ->
    Buffer.add_string buf @@ create_asm'_args [(x, reg_cl)] ys zs;
    let ss = stacksize () in
    if ss > 0 then Buffer.add_string buf @@ Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp;
    Buffer.add_string buf @@ Printf.sprintf "\tcall\t*(%s)\n" reg_cl;
    if ss > 0 then Buffer.add_string buf @@ Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp;
    (if List.mem a allregs && a <> regs.(0) then
       Buffer.add_string buf @@ Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a
     else if List.mem a allfregs && a <> fregs.(0) then
       Buffer.add_string buf @@ Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a);
    Buffer.contents buf
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    Buffer.create 100
    |> fun buf ->
    let ss = stacksize () in
    Buffer.add_string buf @@ create_asm'_args [] ys zs;
    if ss > 0 then Buffer.add_string buf @@ Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp;
    Buffer.add_string buf @@ (
      if Core.String.exists x ~f:(String.contains "interp") then
        Printf.sprintf "\tcall\t%s\n" "min_caml_mid_layer"
      else
        Printf.sprintf "\tcall\t%s\n" x);
    if ss > 0 then Buffer.add_string buf @@ Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp;
    (if List.mem a allregs && a <> regs.(0) then
       Buffer.add_string buf @@ Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a
     else if List.mem a allfregs && a <> fregs.(0) then
       Buffer.add_string buf @@ Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a);
    Buffer.contents buf

and create_asm'_tail_if e1 e2 b bn =
  let buf = Buffer.create 1000 in
  let b_else = Id.genid (b ^ "_else") in
  Printf.sprintf "\t%s\t%s\n" bn b_else |> Buffer.add_string buf;
  let stackset_back = !stackset in
  create_asm (Tail, e1) |> Buffer.add_string buf;
  Printf.sprintf "%s:\n" b_else |> Buffer.add_string buf;
  stackset := stackset_back;
  create_asm (Tail, e2) |> Buffer.add_string buf;
  Buffer.contents buf

and create_asm'_non_tail_if dest e1 e2 b bn =
  let buf = Buffer.create 1000 in
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.sprintf "\t%s\t%s\n" bn b_else |> Buffer.add_string buf;
  let stackset_back = !stackset in
  create_asm (dest, e1) |> Buffer.add_string buf;
  let stackset1 = !stackset in
  Printf.sprintf "\tjmp\t%s\n" b_cont |> Buffer.add_string buf;
  Printf.sprintf "%s:\n" b_else |> Buffer.add_string buf;
  stackset := stackset_back;
  create_asm (dest, e2) |> Buffer.add_string buf;
  Printf.sprintf "%s:\n" b_cont |> Buffer.add_string buf;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2;
  Buffer.contents buf

and create_asm'_args x_reg_cl ys zs =
  let buf = Buffer.create 100 in
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys
  in
  List.iter
    (fun (y, r) -> Printf.sprintf "\tmovl\t%s, %s\n" y r |> Buffer.add_string buf)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs
  in
  List.iter
    (fun (z, fr) -> Printf.sprintf "\tmovsd\t%s, %s\n" z fr |> Buffer.add_string buf)
    (shuffle sw zfrs);
  Buffer.contents buf

let create_asm_fundef fundef =
  let { name = Id.L (x); body } = RegAlloc.h fundef in
  let buf = Buffer.create 1000 in
  stackset := S.empty;
  stackmap := [];
  Printf.sprintf ".globl %s\n" x |> Buffer.add_string buf;
  Printf.sprintf "%s:\n" x |> Buffer.add_string buf;
  create_asm (Tail, body) |> Buffer.add_string buf;
  buf

let emit_midlayer tr (file : string) (interp : string) : Buffer.t =
  let buf = Buffer.create 1000 in
  Printf.sprintf ".globl min_caml_trace_entry\n" |> Buffer.add_string buf;
  Printf.sprintf "min_caml_trace_entry:\n" |> Buffer.add_string buf;
  Printf.sprintf "\tpushl\t%%eax\n" |> Buffer.add_string buf;
  Printf.sprintf "\tpushl\t%%ecx\n" |> Buffer.add_string buf;
  Printf.sprintf "\tcall\tmin_caml_test_trace\n" |> Buffer.add_string buf;
  Printf.sprintf "\tpopl\t%%edx\n" |> Buffer.add_string buf;
  Printf.sprintf "\tpopl\t%%edx\n" |> Buffer.add_string buf;
  Printf.sprintf "\tret\n" |> Buffer.add_string buf;
  Printf.sprintf ".globl min_caml_mid_layer\n" |> Buffer.add_string buf;
  Printf.sprintf "min_caml_mid_layer:\n" |> Buffer.add_string buf;
  begin match tr with
    | `Meta_method ->
      Printf.sprintf "\tmovl\t%d(%%esp), %%eax\n" 12 |> Buffer.add_string buf;
      Printf.sprintf "\tmovl\t%d(%%esp), %%ecx\n" 8 |> Buffer.add_string buf;
    | `Meta_tracing ->
      Printf.sprintf "\tmovl\t%d(%%esp), %%eax\n" 8 |> Buffer.add_string buf;
      Printf.sprintf "\tmovl\t%d(%%esp), %%ecx\n" 4 |> Buffer.add_string buf;
  end;
  Printf.sprintf "\tjmp\t%s\n" interp |> Buffer.add_string buf;
  buf

(* deprecaed. use emit_result *)
let emit_trace jtype trace file interp =
  stackset := S.empty;
  stackmap := [];
  RegAlloc.h trace
  |> fun { name = Id.L (x); body } ->
  let buf = Buffer.create 1000 in
  emit_midlayer jtype file interp |> Buffer.add_buffer buf;
  Printf.sprintf ".globl %s\n" x |> Buffer.add_string buf;
  Printf.sprintf "%s:\n" x |> Buffer.add_string buf;
  create_asm (Tail, body) |> Buffer.add_string buf;
  let res = Buffer.contents buf in
  Logs.debug (fun m -> m  "\n!!EMIT TRACE!!\n%s" res);
  let oc = open_out (file ^ ".s") in
  Printf.fprintf oc "%s" res;
  close_out oc

let emit_result ~jit_type:jtype ~prog:(Prog (_, fundefs, _)) ~traces:trs ~file:f =
  stackset := S.empty;
  stackmap := [];
  let interp_name = find_interp_name fundefs in
  let buf = Buffer.create 1000 in
  List.iter begin fun fundef ->
    create_asm_fundef fundef |> Buffer.add_buffer buf
  end trs;
  emit_midlayer jtype f interp_name
  |> Buffer.add_buffer buf;
  let res = Buffer.contents buf in
  Logs.debug (fun m -> m "\n!!EMIT TRACE!!\n%s)" res);
  let oc = open_out (f ^ ".s") in
  Printf.fprintf oc "%s" res;
  close_out oc
