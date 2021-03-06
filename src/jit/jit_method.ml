open Utils
open MinCaml
open Asm
open Inlining
open Renaming
open Jit_util
open Operands

(* function_name -> (arguments, following expressions) *)
module M = Map.Make (String)

type mj_env = {trace_name: string; red_args: string list; index_pc: int; merge_pc: int}

exception Error of string

let index_pc = ref 0

let merge_pc = ref 0

let print_list f lst =
  print_string "[";
  List.map f lst |> String.concat "; " |> print_string;
  print_string "]"

let empty_fenv () = M.empty

let extend_fenv name args func fenv = M.add name (args, func) fenv

let gen_fname id = Id.genid id

let find_pc args =
  match List.nth_opt args !index_pc with
  | Some v -> int_of_id_t v
  | None ->
     let arg_strings = "[|" ^ (args |> String.concat "; ") ^ "|]" in
     raise (Error (Printf.sprintf "find_pc is failed. index_pc: %d, args: %s" !index_pc arg_strings))

let rec restore_args cont reg = function
  | [] -> cont
  | hd :: tl ->
      let jv = reg.(int_of_id_t hd) in
      if is_green jv then
        Let ((hd, Type.Int), Set (value_of jv), restore_args cont reg tl)
      else restore_args cont reg tl

let rec mj p reg mem fenv name = function
  | Ans exp -> mj_exp p reg mem fenv name exp
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_start", args, fargs), body) ->
      Log.debug (Printf.sprintf "min_caml_loop_start") ;
      let fname = gen_fname "loop_start" in
      let extended_fenv = extend_fenv fname args body fenv in
      (Ans (CallDir (Id.L fname, args, fargs)), extended_fenv)
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_end", args, fargs), body) ->
      (* [TODO] インタプリタに差しもどす処理を入れる *)
      Log.debug (Printf.sprintf "min_caml_loop_end.") ;
      (Ans (CallDir (Id.L name, args, fargs)), M.empty)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_method_entry"), args, fargs), body) ->
     Log.debug ("min_caml_method_entry");
     mj p reg mem fenv name body
  | Let ((dest, typ), CallDir (Id.L ("min_caml_jit_merge_point"), args, fargs), body) ->
     let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
     Log.debug (Printf.sprintf "jit_merge_point pc: %d" pc);
     mj p reg mem fenv name body
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
      (* [TODO] pc の値が method の先頭でかつ
       * interpreter 呼び出しの場合のみ call するように変更 *)
      let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
      let callee_id = if pc = !merge_pc then Id.L name else id_l in
      let restored_call =
        restore_args
          (Let ((dest, typ), CallDir (callee_id, args, fargs), Ans Nop))
          reg args
      in
      let t, x = mj p reg mem fenv name body in
      (connect (Id.gentmp Type.Int) restored_call t, x)
  | Let ((dest, typ), exp, body) -> (
    match exp with
    | IfEq _ | IfGE _ | IfLE _ ->
        let t' = mj_if p reg mem fenv name exp in
        let k = mj p reg mem fenv name body in
        (connect dest (fst t') (fst k), snd k)
    | St (id_t1, id_t2, id_or_imm, x) -> (
        let srcv = reg.(int_of_id_t id_t1) in
        let destv = reg.(int_of_id_t id_t2) in
        let offsetv =
          match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
        in
        let body', m' = mj p reg mem fenv name body in
        match (srcv, destv) with
        | Green n1, Red n2 | LightGreen n1, Red n2 -> (
            reg.(int_of_id_t dest) <- Green 0 ;
            mem.(n1 + (n2 * x)) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of) ;
            match offsetv with
            | Green n | LightGreen n ->
                let id' = Id.gentmp Type.Int in
                ( Let
                    ( (id_t1, Type.Int)
                    , Set n1
                    , Let
                        ( (id', Type.Int)
                        , Set n
                        , Let ((dest, typ), St (id_t1, id_t2, C n, x), body') ) )
                , m' )
            | Red n ->
                ( Let
                    ( (id_t1, Type.Int)
                    , Set n1
                    , Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body') )
                , m' ) )
        | _ -> optimize_exp p exp reg mem fenv name (dest, typ) body )
    | _ -> optimize_exp p exp reg mem fenv name (dest, typ) body )

and optimize_exp p exp reg mem fenv name (dest, typ) body =
  match Jit_optimizer.run p exp reg mem with
  | Specialized v ->
      reg.(int_of_id_t dest) <- v ;
      mj p reg mem fenv name body
  | Not_specialized (e, v) ->
      reg.(int_of_id_t dest) <- v ;
      let t, x = mj p reg mem fenv name body in
      (Let ((dest, typ), e, t), x)

and mj_exp p reg mem fenv name = function
  | CallDir (id_l, args, fargs) ->
      Log.debug (Printf.sprintf "CallDir (%s)" (string_of_id_l id_l)) ;
      let fundef = find_fundef p id_l in
      let t = Inlining.inline_fundef reg args fundef in
      mj p reg mem fenv name t
  | (IfEq _ | IfGE _ | IfLE _) as exp -> mj_if p reg mem fenv name exp
  | exp -> (
    match Jit_optimizer.run p exp reg mem with
    | Specialized v ->
        let id = Id.gentmp Type.Int in
        (Let ((id, Type.Int), Set (value_of v), Ans (Mov id)), M.empty)
    | Not_specialized (e, v) -> (Ans e, M.empty) )

and mj_if p reg mem fenv name = function
  | ( IfGE (id_t, id_or_imm, t1, t2)
    | IfEq (id_t, id_or_imm, t1, t2)
    | IfLE (id_t, id_or_imm, t1, t2) ) as exp
    when is_opcode id_t ->
      Log.debug
        (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm)) ;
      let r1 = value_of reg.(int_of_id_t id_t) in
      let r2 = match id_or_imm with V id -> value_of reg.(int_of_id_t id) | C n -> n in
      if exp |*| (r1, r2) then mj p reg mem fenv name t1 else mj p reg mem fenv name t2
  | ( IfEq (id_t, id_or_imm, t1, t2)
    | IfLE (id_t, id_or_imm, t1, t2)
    | IfGE (id_t, id_or_imm, t1, t2) ) as exp -> (
      Log.debug
        (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm)) ;
      let r1 = reg.(int_of_id_t id_t) in
      let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
      let regt1, regt2 = (Array.copy reg, Array.copy reg) in
      let memt1, memt2 = (Array.copy mem, Array.copy mem) in
      let t1' = mj p regt1 memt1 fenv name t1 in
      let t2' = mj p regt2 memt2 fenv name t2 in
      let new_map = M.fold M.add (snd t1') (snd t2') in
      match (r1, r2) with
      | Green n1, Green n2
       |LightGreen n1, LightGreen n2
       |Green n1, LightGreen n2
       |LightGreen n1, Green n2 ->
          if exp |*| (n1, n2) then t1' else t2'
      | Red n1, Green n2 | Red n1, LightGreen n2 ->
          (Ans (exp |%| (id_t, C n2, fst t1', fst t2')), new_map)
      | Green n1, Red n2 | LightGreen n1, Red n2 ->
          let id_t2 =
            match id_or_imm with
            | V id -> id
            | C n -> failwith "id_or_imm should be string"
          in
          (Ans (exp |%| (id_t2, C n1, fst t2', fst t1')), new_map)
      | Red n1, Red n2 -> (Ans (exp |%| (id_t, id_or_imm, fst t1', fst t2')), new_map) )
  | _ -> failwith "method_jit_if should accept conditional branches."

let run_while (Prog (_, fundefs, main) as p) reg mem name reds =
  (* let (Jit_prep.Env (fdfs, ibody, reds)) =
   *   Jit_prep.prep ~prog:p ~name ~red_args:reds ~jit_type:`Meta_method
   * in
   * let p' = Prog (tbl, fdfs, m) in *)
  let rec loop p reg mem fenv name args t =
    let t1, fenv1 = mj p reg mem fenv name t in
    match M.choose_opt fenv1 with
    | Some (n, (ag, bd)) -> (
        ((t1, name, args) :: loop p reg mem M.empty n ag bd)
        @
        match M.choose_opt (M.remove n fenv1) with
        | Some (n', (ag', bd')) -> loop p reg mem M.empty n' ag' bd'
        | None -> [] )
    | None -> [(t1, name, args)]
  in
  let { body } = find_fundef' p "interp" in
  let loops = loop p reg mem M.empty name reds body in
  List.map
    (fun (body, name, args) -> {name= Id.L name; args; fargs= []; body; ret= Type.Int})
    loops

let run prog reg mem {trace_name; red_args; index_pc= x; merge_pc= y} =
  index_pc := x ;
  merge_pc := y ;
  run_while prog reg mem trace_name red_args
