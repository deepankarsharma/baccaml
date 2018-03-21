open Asm
open Core
open OUnit
open Tracing_jit
open Method_jit
open Jit_config
open Mincaml_util
open Test_util

let Prog (_, fundefs, main) as prog =
  In_channel.create (dir ^ "simple3.ml")
  |> Lexing.from_channel
  |> virtualize

let bytecode = [|22; 2; 22; 2; 0; 11; 12; 9; 13; 10; 0; 4; 5; 11|]

let _ = run_test_tt_main begin
    "simple3_test" >::: [
      "method_jit" >::
      begin fun () ->
        let fundef = List.hd_exn fundefs in
        let method_jit_args = {
          method_name = "test_method.1000";
          reds = ["stack.107"; "sp.108"];
          method_start = 0;
          method_end = 6;
          pc_place = 1
        } in
        let { body } = fundef in
        let reg = Array.create 10000 (Red 0) in
        let mem = Array.create 10000 (Red 0) in
        reg.(126) <- Green (0);
        reg.(127) <- Green (6);
        reg.(128) <- Green (100);
        reg.(129) <- Green (1);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 44 then
            mem.(n) <- Red (bytecode.(i))
          else if n = 48 then
            mem.(n) <- Red (bytecode.(i))
          else
            mem.(i * 4) <- Green (bytecode.(i))
        done;
        let res = method_jit prog body reg mem method_jit_args in
        Out_channel.output_string stdout (Emit_virtual.to_string_t res);
        Out_channel.newline stdout;
        let trace = {
          name = Id.L ("test_trace.1000");
          args = [];
          fargs = [];
          body =res;
          ret = Type.Int
        } in
        let prog' = Prog ([], trace :: fundefs, main) in
        Jit_compiler.compile prog' "test/simple3.s";
        ()
      end;
      "tracing_jit" >::
      begin fun () ->
        let { body } = List.hd_exn fundefs in
        let tracing_jit_args = {
          trace_name = "test_trace.1000";
          reds = ["stack.107"; "sp.108"];
          greens = [];
          loop_header = 6;
          loop_pc_place = 1;
        } in
        let reg = Array.create 100000 (Red 0) in
        let mem = Array.create 100000 (Red 0) in
        reg.(126) <- Green (0);
        reg.(127) <- Green (6);
        reg.(128) <- Green (100);
        reg.(129) <- Green (1);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 44 then
            mem.(n) <- Red (bytecode.(i))
          else if n = 48 then
            mem.(n) <- Red (bytecode.(i))
          else
            mem.(i * 4) <- Green (bytecode.(i))
        done;
        let res = Tracing_jit.tracing_jit prog body reg mem tracing_jit_args in
        let trace = {
          name = Id.L ("test_trace.1000");
          args = [];
          fargs = [];
          body =res;
          ret = Type.Int
        } in
        let prog' = Prog ([], trace :: fundefs, main) in
        (* prog' |> Simm.f |> RegAlloc.f |> Emit.f (Out_channel.create ("test/pypyfig3.s")); *)
        print_string (Emit_virtual.to_string_t res);
      end
    ]
  end
