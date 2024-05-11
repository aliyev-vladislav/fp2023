open DAliev_lib
open Format

let () =
let s = Stdio.In_channel.input_all Stdlib.stdin in
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Infer.run_inference parsed with
     | Ok env_inf ->
       (match Interpret.InterpretResult.eval_program parsed with
        | Ok env_int -> Interpret.pp_env env_inf env_int
        | Error e -> printf "Interpreter error: %a\n" Interpret.pp_error e)
     | Error e -> printf "Infer error: %a\n" Typing.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
