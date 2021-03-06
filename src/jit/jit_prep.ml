open MinCaml
open Asm
open Jit_util

exception Error of string

type env = Env of Asm.fundef list * Asm.t * Id.t list

let create_reds reds (Prog (_, fundefs, _)) =
  let interp =
    List.find
      (fun {name= Id.L x} -> String.split_on_char '.' x |> List.hd = "interp")
      fundefs
  in
  let {args} = interp in
  List.filter
    (fun arg ->
      let arg_name = String.split_on_char '.' arg |> List.hd in
      List.exists (fun s -> s = arg_name) reds )
    args

let prep' p t =
  match Simm.t t |> Jit_trim.trim with
  | Let
      ( _
      , Set _
      , Let
          ( _
          , IfEq (_, _, _, _)
          , Let (_, CallDir (Id.L _, args, fargs), interp_body) ) )
  | Let
      ( _
      , IfEq (_, _, _, _)
      , Let (_, CallDir (Id.L _, args, fargs), interp_body) )
  | Ans (IfEq (_, _, Ans (CallDir (Id.L _, args, fargs)), interp_body)) ->
      let (Prog (table, fundefs, main)) = p in
      let fundefs' =
        fundefs
        |> List.map (fun fundef ->
               let (Id.L x) = fundef.name in
               match String.split_on_char '.' x |> List.hd with
               | name' when name' = "interp" ->
                   let {name; args; fargs; ret} = fundef in
                   {name; args; fargs; body= interp_body; ret}
               | _ -> fundef )
      in
      (fundefs', interp_body)
  | _ ->
      raise @@ Error "missing `jit_dispatch' at the top of your interpreter."

let prep ~prog:p ~name:n ~red_args:reds ~jit_type:jtyp =
  let (Prog (table, fundefs, main)) = p |> Jit_annot.annotate jtyp in
  let {body} = find_fundef' p "interp" in
  let fundefs', interp_body = prep' p body in
  Env (fundefs', interp_body, create_reds reds p)
