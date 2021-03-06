open MinCaml
open Asm
open Bc_jit
open Jit_util
open Operands

type env =
  { prog: prog
  ; reg: value array
  ; mem: value array
  ; red_args: string list
  ; ex_name: string
  ; merge_pc: int
  ; trace_name: string }

type arg =
  { file: string
  ; ex_name: string
  ; code: int array
  ; annot: int array
  ; reds: (string * int) list
  ; greens: (string * int) list
  ; merge_pc: int
  ; trace_name: string }

type var = {redtbl: (string, int) Hashtbl.t; greentbl: (string, int) Hashtbl.t}

type tenv = {fundefs: fundef list; ibody: Asm.t}

exception Error of string


let get_name x = String.split_on_char '.' x |> List.hd


let annot is_mj (Prog (table, fundefs, main) as p) =
  let rec annot' is_mj = function
    | Ans e -> (
      match e with
      | IfEq (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfLE (x, y, t1, t2) ->
          Ans (e |%| (x, y, annot' is_mj t1, annot' is_mj t2))
      | IfFLE (x, y, t1, t2) | IfFEq (x, y, t1, t2) ->
          Ans (e |%| (x, V y, annot' is_mj t1, annot' is_mj t2))
      | _ -> Ans e )
    | Let (x, CallDir (id_l, args, fargs), t) when id_l = Id.L "min_caml_is_mj"
    -> (
      match t with
      | Ans (IfEq (_, _, t1, t2)) -> (
        (* if is_mj () then t1 else t2 is compiled to *)
        (* IfEq((x, 0, t2, t1)                        *)
        match is_mj with
        | `Meta_method -> t2
        | `Meta_tracing -> t1 )
      | _ -> Let (x, CallDir (id_l, args, fargs), annot' is_mj t) )
    | Let (r, x, t) -> Let (r, x, annot' is_mj t)
  in
  let {name; args; fargs; body; ret} = find_fundef' p "interp" in
  let other_fundefs =
    List.filter (fun fundef -> fundef.name <> name) fundefs
  in
  let new_fundefs =
    {name; args; fargs; ret; body= annot' is_mj body} :: other_fundefs
  in
  Prog (table, new_fundefs, main)


let prepare_reds trace_args (Prog (_, fundefs, _) as p) =
  let {args} = find_fundef p (Id.L "interp") in
  args
  |> List.filter (fun arg ->
         trace_args |> List.exists (fun trace_arg -> trace_arg = get_name arg)
       )


let prepare_var red_lst green_lst =
  let red_tbl = Hashtbl.create 10 in
  let green_tbl = Hashtbl.create 10 in
  List.iter (fun r -> Hashtbl.add red_tbl (fst r) (snd r)) red_lst ;
  List.iter (fun g -> Hashtbl.add green_tbl (fst g) (snd g)) green_lst ;
  {redtbl= red_tbl; greentbl= green_tbl}


let prepare_prog bytecode addr annot mem =
  for i = 0 to Array.length bytecode - 1 do
    if Array.exists (fun annot -> annot = i) annot then
      mem.(addr + (i * 4)) <- Red bytecode.(i)
    else mem.(addr + (i * 4)) <- Green bytecode.(i)
  done


let prepare_tenv ~prog:p ~name:n ~red_args:reds =
  let (Prog (table, fundefs, main)) = p in
  let {body} =
    List.find
      (fun {name= Id.L x; _} ->
        String.split_on_char '.' x |> List.hd |> contains "interp" )
      fundefs
  in
  match Simm.t body |> Jit_trim.trim with
  | Let
      ( _
      , Set _
      , Let
          ( _
          , IfEq (_, _, _, _)
          , Let (_, CallDir (id_l, args, fargs), interp_body) ) )
   |Let
      (_, IfEq (_, _, _, _), Let (_, CallDir (id_l, args, fargs), interp_body))
   |Ans (IfEq (_, _, Ans (CallDir (id_l, args, fargs)), interp_body)) ->
      { fundefs=
          List.map
            (fun fundef ->
              let (Id.L x) = fundef.name in
              match get_name x with
              | name' when name' = "interp" ->
                  let {name; args; fargs; ret} = fundef in
                  {name; args; fargs; body= interp_body; ret}
              | _ -> fundef )
            fundefs
      ; ibody= interp_body }
  | _ -> raise (Error "Missing hint function: jit_dispatch")


let prepare_env jit_type
    {file; ex_name; code; annot; reds; greens; merge_pc; trace_name} =
  let p =
    open_in file |> Lexing.from_channel |> Util.virtualize |> Simm.f
    |> Jit_annot.annotate jit_type
  in
  let reg = Array.make 10000000 (Red 0) in
  let mem = Array.make 10000000 (Red 0) in
  let red_args = List.map fst reds in
  let tenv = prepare_tenv ~prog:p ~name:trace_name ~red_args in
  let {greentbl; redtbl} = prepare_var reds greens in
  Colorizer.colorize_reg redtbl greentbl reg (List.hd tenv.fundefs) tenv.ibody ;
  let addr =
    match
      greens
      |> List.find_opt (fun arg' -> fst arg' = "bytecode" || fst arg' = "code")
    with
    | Some x -> x |> snd
    | None -> (
      match
        reds
        |> List.find_opt (fun arg' ->
               fst arg' = "bytecode" || fst arg' = "code" )
      with
      | Some x -> x |> snd
      | None -> raise Not_found )
  in
  prepare_prog code addr annot mem ;
  {prog= p; reg; mem; red_args; ex_name; merge_pc; trace_name}
