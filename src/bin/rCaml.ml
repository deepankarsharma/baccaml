open MinCaml
open Asm
open BacCaml
open Jit_util
open Jit_prep

open Fieldslib

exception Error of string

type arg = {
  file : string;
  main : string;
  ex_name : string;
  code : int array;
  annot : int array;
  reds : (string * int) list;
  greens : (string * int) list;
} [@@deriving fields]

type var = {
  redtbl : (string, int) Hashtbl.t;
  greentbl : (string, int) Hashtbl.t;
} [@@deriving fields]

let prepare_var red_lst green_lst =
  let red_tbl = Hashtbl.create 10 in
  let green_tbl = Hashtbl.create 10 in
  List.iter
    (fun r -> Hashtbl.add red_tbl (fst r) (snd r))
    red_lst;
  List.iter
    (fun g -> Hashtbl.add green_tbl (fst g) (snd g))
    green_lst;
  { redtbl = red_tbl; greentbl = green_tbl }

let prepare_env jit_type arg =
  let interp =
    open_in (Fieldslib.(file arg))
    |> Lexing.from_channel
    |> Util.virtualize_fundefs
    |> List.map Simm.h
    |> List.map (Jit_annot.gen_mj_fundef jit_type)
  in
  let p =
    let Prog (tbl, _, main) =
      open_in (Fieldslib.(main arg))
      |> Lexing.from_channel
      |> Util.virtualize
      |> Simm.f in Prog (tbl, interp, main)

  in
  let reg, mem = Array.make 100000 (Red (0)), Array.make 100000 (Red (0)) in

  let red_args = List.map fst (Fieldslib.(reds arg)) in
  let tenv = prepare_tenv ~fundefs:interp ~red_args:red_args in
  (prepare_var (Fieldslib.(reds arg)) (Fieldslib.(greens arg))
   |> fun var ->
   Colorizer.colorize_reg
     (Fieldslib.(redtbl var))
     (Fieldslib.(greentbl var))
     reg
     (List.hd (Fieldslib.(fundefs tenv)))
     (Fieldslib.(ibody tenv));
   let addr =
     match
       Fieldslib.(greens arg)
       |> List.find_opt (fun arg' -> fst arg' = "bytecode")
     with
     | Some x -> x |> snd
     | None ->
       match
         Fieldslib.(reds arg)
         |> List.find_opt (fun arg' -> fst arg' = "bytecode")
       with
       | Some x -> x |> snd
       | None -> raise Not_found
   in
   Jit_prep.prepare_prog (Fieldslib.(code arg)) addr (Fieldslib.(annot arg)) mem);
   Fieldslib.(
    Fields_of_env.create
      ~prog:p
      ~fundefs':interp
      ~reg:reg
      ~mem:mem
      ~red_args:red_args
      ~ex_name:(ex_name arg))


module Util = struct
  let to_tuple lst =
    if List.length lst = 0 then
      [("dummy", "0")]
    else if List.length (List.hd lst) <> 2 then
      failwith "to_tuple: element of list's size should be 2."
    else
      List.map (fun elm -> (List.nth elm 0, List.nth elm 1)) lst

  let string_of_array ~f str_lst =
    str_lst
    |> Str.split_delim (Str.regexp " ")
    |> List.map f
    |> Array.of_list

  (* parse a list like "a 1; b 2" -> [("a", 1), ("b", 2)] *)
  let parse_pair_list pair_lst =
    pair_lst
    |> Str.split_delim (Str.regexp "; ")
    |> List.map (Str.split_delim (Str.regexp " "))
    |> to_tuple
    |> List.map (fun (x, y) -> (x, int_of_string y))

  let print_list f lst =
    let rec loop f = function
      | [] -> ()
      | hd :: tl -> f hd; print_string "; "; loop f tl
    in
    print_string "["; loop f lst; print_string "]"
end

exception Jittype_error of string

let file      = ref ""
let main      = ref ""
let codes     = ref ""
let annots    = ref ""
let reds      = ref ""
let greens    = ref ""
let output    = ref "out"
let jittype   = ref ""

let usage  = "usage: " ^ Sys.argv.(0) ^ " [-file string] [-green string list] [-red string list] [-code int list] [-annot int list]"

let speclist = [
  ("-file", Arg.Set_string file, "Specify file name");
  ("-main", Arg.Set_string main, "Specify main file name");
  ("-green", Arg.Set_string greens, "Specify green variables");
  ("-red", Arg.Set_string reds, "Specify red variables");
  ("-code", Arg.Set_string codes, "Specify bytecode");
  ("-annot", Arg.Set_string annots, "Specify annotations for bytecode");
  ("-type", Arg.Set_string jittype, "Specify jit type");
  ("-o", Arg.Set_string output, "Set executable's name");
  ("-dbg", Arg.Unit (fun _ -> Logs.set_level @@ Some Logs.Debug), "Enable debug mode");
]

let run = (fun f ->
    Arg.parse
      speclist
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage;
    Logs.set_reporter @@ Logs.format_reporter ();
    let file = !file in
    let main = !main in
    let output = !output in
    let bytes = Util.string_of_array ~f:int_of_string !codes in
    let annots = Util.string_of_array ~f:int_of_string !annots in
    let reds = Util.parse_pair_list !reds in
    let greens = Util.parse_pair_list !greens in
    let jittype' = match !jittype with
      | "tjit" -> `Meta_tracing
      | "mjit" -> `Meta_method
      | _ -> raise @@ Jittype_error "-type (tjit|mjit) is missing."
    in
    f jittype' Fieldslib.(
        Fields_of_arg.create
          ~file:file ~main:main ~ex_name:output
          ~code:bytes ~annot:annots ~reds:reds ~greens:greens
      ))
