open Core
open OUnit

open MinCaml
open Asm

open Bc_jit
open Jit_config

module TJ = Tracing_jit
module JE = Jit_emit

let jit_args =
  Tracing_jit_args (
    { trace_name = "min_caml_test_trace"
    ; reds = ["ytecode.43"; "a.45"]
    ; greens = []
    ; loop_header = 0
    ; loop_pc_place = 1 })

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "simple1_test" >:: begin fun () ->
        let prog =
          In_channel.create ("simple1.ml")
          |> Lexing.from_channel
          |> Util.virtualize
          |> Simm.f
        in
        let Prog (_, fundefs, main) = prog in
        let fundef = List.hd_exn fundefs in
        let { body; } = fundef in
        let reg = Array.make 1000 (Red 0) in
        let mem = Array.make 1000 (Red 0) in
        mem.(0 * 4) <- Green (1);
        mem.(1 * 4) <- Green (2);
        mem.(2 * 4) <- Green (0);
        mem.(3 * 4) <- Green (4);
        reg.(43) <- Green (0);
        reg.(44) <- Green (0);
        reg.(45) <- Red (100);
        let res = match TJ.exec prog body reg mem jit_args with
            Tracing_success v | Method_success v -> v
        in
        JE.emit_trace `Meta_tracing res "simple1_tj" "interp.42"
      end
    ]
  end
