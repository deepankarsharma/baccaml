open MinCaml
open Asm
open Operands

exception Error

let rec gen_mj_t is_mj t = match t with
  | Ans (e) ->
    begin match e with
      | IfEq (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfLE (x, y, t1, t2) ->
        Ans (e |%| (x, y, gen_mj_t is_mj t1, gen_mj_t is_mj t2))
      | IfFLE (x, y, t1, t2) | IfFEq (x, y, t1, t2) ->
        Ans (e |%| (x, V (y), gen_mj_t is_mj t1, gen_mj_t is_mj t2))
      | _ -> Ans (e)
    end
  | Let (x, CallDir (id_l, args, fargs), t) when id_l = (Id.L ("min_caml_is_mj"))->
    begin match t with
      | Ans (IfEq (_, _, t1, t2)) ->
        (* if is_mj () then t1 else t2 is compiled to *)
        (* IfEq((x, 0, t2, t1)                        *)
        begin
          match is_mj with
          | `Meta_method -> t2
          | `Meta_tracing -> t1
        end
      | _ ->
        Let (x, CallDir (id_l, args, fargs), gen_mj_t is_mj t)
    end
  | Let (r, x, t) ->
    Let (r, x, gen_mj_t is_mj t)

let gen_mj_fundef is_mj { name; args; fargs; body; ret } =
  { name = name; args = args; fargs = fargs; ret = ret;
    body = gen_mj_t is_mj body }

let gen_mj is_mj (Prog (table, fundefs, main)) =
  let interp_fundef = find_fundef "min_caml_interp" fundefs in
  let other_fundefs = List.filter (fun fundef -> fundef.name <> interp_fundef.name) fundefs in
  let new_fundefs = (gen_mj_fundef is_mj interp_fundef) :: other_fundefs in
  Prog (table, new_fundefs, main)
