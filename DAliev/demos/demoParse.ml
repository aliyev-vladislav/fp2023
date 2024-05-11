open DAliev_lib

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse str with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast 
  | Error s -> Format.printf "Some error: %s\n" s
;;

