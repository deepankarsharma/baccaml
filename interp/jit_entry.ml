open Utils
open Std
open MinCaml
open Asm
open Bc_jit
open Jit_util

(* For test *)
let dummy_fun x =
  print_string "apply dummy_fun to " ;
  print_int x ;
  print_newline () ;
  print_string "executed file is " ;
  print_endline Sys.argv.(0) ;
  x + 1

let size = 100000

(* TODO: specify extenally *)
let greens = ["pc"; "bytecode"]

let bc_tmp_addr = 0

let st_tmp_addr = 100

let get_ir_addr args name =
  args
  |> List.find (fun a -> String.get_name a = name)
  |> String.get_extension |> int_of_string

let make_reg prog args sp =
  let reg = Array.make size (Red 0) in
  let {args; body= t} = find_fundef' prog "interp" in
  fv t @ args
  |> List.iteri (fun i a ->
         if List.mem (String.get_name a) greens then reg.(i) <- Green 0
         else reg.(i) <- Red 0 ) ;
  reg

let make_mem bytecode stack =
  let mem = Array.make size (Green 0) in
  bytecode |> Array.iteri (fun i a -> mem.(bc_tmp_addr + (4 * i)) <- Green a) ;
  stack |> Array.iteri (fun i a -> mem.(st_tmp_addr + (4 * i)) <- Red a) ;
  mem

let jit_entry bytecode stack pc sp bc_ptr st_ptr =
  Array.print_array print_int bytecode ;
  print_newline () ;
  Array.print_array print_int stack ;
  print_newline () ;
  Printf.eprintf "pc %d, sp %d, bc_ptr %d, st_ptr %d\n" pc sp bc_ptr st_ptr ;
  let prog =
    let ic = open_in "./test_interp.mcml" in
    try
      let v = ic |> Lexing.from_channel |> Util.virtualize in
      close_in ic ; v
    with e -> close_in ic ; raise e
  in
  let {args; body} = find_fundef' prog "interp" in
  let reg = make_reg prog args sp in
  let mem = make_mem bytecode stack in
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  reg.(pc_ir_addr) <- Green pc ;
  reg.(sp_ir_addr) <- Red sp ;
  reg.(bc_ir_addr) <- Green bc_tmp_addr ;
  reg.(st_ir_addr) <- Red st_tmp_addr ;
  Jit_tracing.(
    let env =
      { index_pc= 3
      ; merge_pc= pc
      ; trace_name= "test_trace"
      ; red_args=
          args
          |> List.filter (fun a -> not (List.mem (String.get_name a) greens))
      }
    in
    let trace = Jit_tracing.run prog reg mem env in
    print_endline (Emit_virtual.string_of_fundef trace)) ;
  ()

let () =
  Callback.register "jit_entry" jit_entry ;
  Callback.register "dummy_fun" dummy_fun
