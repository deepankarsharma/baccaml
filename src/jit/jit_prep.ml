open MinCaml
open Asm
open Jit_util

exception Error of string

type jit_env = Jit_env of fundef list * t * string list

type env = {
  prog : prog;
  fundefs' : fundef list;
  reg : value array;
  mem : value array;
  red_args : string list;
  ex_name : string;
} [@@deriving fields]

type tenv = {
  fundefs : fundef list;
  ibody : Asm.t;
} [@@deriving fields]

let prepare_tenv ~fundefs:fs ~red_args:reds =
  let { body } =
    begin
      match
        List.find_opt begin fun { name = Id.L (x) } ->
          String.split_on_char '.' x
          |> List.hd
          |> String.equal "min_caml_interp"
        end fs
      with
      | Some v -> v
      | None ->
        (List.iter (fun { name = Id.L (x)} -> Printf.eprintf "%s\n" x; ) fs;
         raise Not_found)
    end
  in
  begin match Simm.t body |> Jit_trim.trim with
    | Let (_, Set (_), Let (_,  IfEq (_, _, _, _), Let (_, CallDir (id_l, args, fargs), interp_body)))
    | Let (_,  IfEq (_, _, _, _), Let (_, CallDir (id_l, args, fargs), interp_body))
    | Ans (IfEq (_, _, Ans (CallDir (id_l, args, fargs)), interp_body)) ->
      let fundefs' = List.map begin fun fundef ->
          let Id.L (x) = fundef.name in
          match String.split_on_char '.' x |> List.hd with
          | name' when name' = "min_caml_interp" ->
            let { name; args; fargs; ret } = fundef in
            { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
          | _ -> fundef
        end fs
      in
      Fields_of_tenv.create ~fundefs:fundefs' ~ibody:interp_body
    | _ ->
      failwith "missing jit_dispatch."
  end

let prepare_prog bytecode addr annot mem =
  for i = 0 to (Array.length bytecode - 1) do
    if Array.exists (fun annot -> annot = i) annot then
      mem.(addr + i * 4) <- Red (bytecode.(i))
    else
      mem.(addr + i * 4) <- Green (bytecode.(i))
  done

let prep ~fundefs:fs ~name:n ~red_args:reds ~jit_type:jtyp =
  let create_mj_reds reds fundefs =
    let interp = List.find (fun { name = Id.L (name) } ->
        (String.split_on_char '.' name
         |> List.hd
         |> String.equal "min_caml_interp")) fundefs
    in
    let { args } = interp in
    List.filter begin fun arg ->
      let name = String.split_on_char '.' arg |> List.hd in
      List.exists (fun s -> s = name) reds
    end args
  in
  let { fundefs; ibody } = prepare_tenv ~fundefs:fs ~red_args:reds in
  Jit_env (fundefs, ibody, (create_mj_reds reds fs))
