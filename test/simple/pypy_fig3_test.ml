open Asm
open Core
open OUnit
open Jit_config
open Tracing_jit
open MinCaml_util

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "pypyfig3_test" >::
      begin fun () ->
        let prog =
          In_channel.create (Test_util.dir ^ "pypyfig3.ml")
          |> Lexing.from_channel
          |> virtualize
        in
        let Prog (_, fundefs, main) = prog in
        let fundef = match fundefs with
          | [fundef] -> fundef
          | _ -> failwith "Error."
        in
        let instr = fundef.body in
        let reg = Array.make 1000 (Red 0) in
        let mem = Array.make 1000 (Red 0) in
        let bytecode =
          [|1; 0; 1; 2; 2; 0; 4; 1; 0; 2; 2; 3; 1; 1; 2; 2; 0; 0; 4; 2; 2; 5|]
        in
        for i = 0 to (Array.length bytecode - 1) do
          mem.(i * 4) <- Green (bytecode.(i))
        done;
        reg.(107) <- Green (0);
        reg.(108) <- Green (4);
        reg.(109) <- Red (100);
        reg.(110) <- Green (400);
        mem.(100 * 4) <- Red (100);
        mem.(100 * 4 + 16) <- Red (100);
        mem.(100 * 4 + 32) <- Red (100);
        enable_jit := true;
        Logger.log_level := Logger.Debug;
        let jit_args =
          { trace_name = "test_trace.1000"
          ; reds = ["a.109"; "reg.110"]
          ; greens = []
          ; loop_header = 4
          ; loop_pc_place = 1 }
        in
        let trace = exec_tracing_jit prog instr reg mem jit_args in
        let prog' = Prog ([], trace :: [], Ans(Nop)) in
        let reg' = Array.make 10000 0 in
        let mem' = Array.make 10000 0 in
        Out_channel.print_endline (Emit_virtual.to_string_fundef trace);
        (* Jit_compiler.compile prog' "test/pypy_fig3.s"; *)
      end
    ]
  end
