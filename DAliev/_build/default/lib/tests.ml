open DAliev_lib 

module ParserTests = struct 
  let parse_test str rez = 
    match Parser.parse_input str with 
    | Ok factual -> List.equal Ast.equal_decl rez factual
    | Error err -> 
      Format.printf "%s\n" err; 
      false
  ;;
  let%test _ = parse_test "let f p = p" [(DLet (NonRec, ((PId "f"), (EFun ((PId "p"), (EId "p"))))))]

  let%test _ = 
    parse_test 
      "let v = [2 * 2; 2 + 2; 2 - 2; 2 / 2]" 
      [(DLet (NonRec,                     
        ((PId "v"),
        (ECons ((EBinOp (Mult, (EConst (CInt 2)), (EConst (CInt 2)))),
            (ECons ((EBinOp (Add, (EConst (CInt 2)), (EConst (CInt 2)))),
              (ECons ((EBinOp (Sub, (EConst (CInt 2)), (EConst (CInt 2)))),
                  (ECons ((EBinOp (Div, (EConst (CInt 2)), (EConst (CInt 2)))),
                    (EConst CNil)))
                  ))
              ))
            )))
        ))
      ]
  ;;

  let%test _ = 
    parse_test 
      "let rec fac n = if n < 1 then 1 else n * fac (n - 1)"
      [(DLet (Rec,                        
        ((PId "fac"),
        (EFun ((PId "n"),
            (EIf ((EBinOp (Lt, (EId "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
              (EBinOp (Mult, (EId "n"),
                  (EApp ((EId "fac"),
                    (EBinOp (Sub, (EId "n"), (EConst (CInt 1))))))
                  ))
              ))
            )))
        ))
      ]
  ;;

  let%test _ = 
    parse_test 
      "let v = match l with | h1 :: h2 :: tl -> if h1 <= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0 ;;"
      [(DLet (NonRec,                     
        ((PId "v"),
        (EMatch ((EId "l"),
            [((PCons ((PId "h1"), (PCons ((PId "h2"), (PId "tl"))))),
              (EIf ((EBinOp (Lte, (EId "h1"), (EId "h2"))), (EId "h1"),
                (EId "h2"))));
              ((PCons ((PId "h1"), (PConst CNil))), (EId "h1"));
              (PWild, (EConst (CInt 0)))]
            )))
        ))
      ]
  ;;

  let%test _ = 
    parse_test "let v = [1; 2], [3, 4] ;;"
    [(DLet (NonRec,                     
      ((PId "v"),
      (ETuple
          [(ECons ((EConst (CInt 1)),
              (ECons ((EConst (CInt 2)), (EConst CNil)))));
            (ECons ((ETuple [(EConst (CInt 3)); (EConst (CInt 4))]),
              (EConst CNil)))
            ]))
      ))
    ]
  ;;

  let%test _ = 
    parse_test "let () = () in ()"
    [(DLet (NonRec, ((PConst CUnit), (EConst CUnit))))]
  ;;

  let%test _ = 
    parse_test "let rec fac n = if n < 1 then 1 else n * fac (n - 1) ;;"
    [(DLet (Rec,                        
      ((PId "fac"),
      (EFun ((PId "n"),
          (EIf ((EBinOp (Lt, (EId "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
            (EBinOp (Mult, (EId "n"),
                (EApp ((EId "fac"),
                  (EBinOp (Sub, (EId "n"), (EConst (CInt 1))))))
                ))
            ))
          )))
      ))
    ]
  ;;

  let%test _ =
    parse_test "let f (e1, e2) = e1 + e2 ;;"
    [(DLet (NonRec,                     
      ((PId "f"),
      (EFun ((PTuple [(PId "e1"); (PId "e2")]),
          (EBinOp (Add, (EId "e1"), (EId "e2"))))))
      ))
    ]
  ;;

  let%test _ = 
    parse_test "let f e1 e2 = f e1 + f e2 ;;"
    [(DLet (NonRec,                     
      ((PId "f"),
      (EFun ((PId "e1"),
          (EFun ((PId "e2"),
            (EBinOp (Add, (EApp ((EId "f"), (EId "e1"))),
                (EApp ((EId "f"), (EId "e2")))))
            ))
          )))
      ))
    ]
  ;;
end

module InferTests = struct 
  let infer_test str = 
    match Parser.parse str with 
    | Ok factual -> 
      let env = Infer.run_inference factual in
      Infer.print_env env 
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test _ = 
    infer_test 
      {| 
        let v = (fun v -> v, "ocaml", true, 123) ;; 
      |};
    [%expect {| val v : 'a -> ('a * string * bool * int) |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
        let rec fix x y = x (fix x) y ;;
      |};
    [%expect {| val fix : (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
        let f x = x :: [1; 2; 3] ;;
      |};
    [%expect {| val f : int -> int list |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
      let rec f x = 
        match x with
        | hd :: hd :: tl -> f tl
        | hd :: [] -> false
        | _ -> true
      ;;
      |};
    [%expect {| val f : 'e list -> bool |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
      let rec fac n = 
        if n <= 1
        then 1
        else (
          let rec f m acc =
            if m <= 1 then acc else f (m - 1) (acc * m)
          in
          f n 1)
      ;;
      |};
    [%expect {| val fac : int -> int |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
        let f = ([1; 2], [3; 4]) :: [([5; 6], [7; 8]); ([9; 10], [11; 12])] ;;
      |};
    [%expect {| val f : (int list * int list) list |}]
  ;;


  let%expect_test _ = 
    infer_test 
      {|
      let f x = x :: [1; 2; 3; "ocaml"; 5];;
      |};
    [%expect {| Infer error: Failed to unify types string and int |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
        let rec f x = f ;;
      |};
    [%expect {| Infer error: Occurs check failed |}]
  ;;

  let%expect_test _ = 
    infer_test 
      {|
        let rec f x = x + y;;
      |};
    [%expect {| Infer error: Unknown variable 'y' |}]
  ;;
end

module InterpretTests = struct
  let interpret_test str = 
  let open Stdlib.Format in
  match Parser.parse str with
  | Ok parsed ->
    (match Infer.run_inference parsed with
     | Ok env_inf ->
       (match Interpret.InterpretResult.eval_program parsed with
        | Ok env_int -> Interpret.pp_env env_inf env_int
        | Error e -> printf "Interpreter error: %a\n" Interpret.pp_error e)
     | Error e -> printf "Infer error: %a\n" Typing.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e

  let%expect_test _ = 
    interpret_test 
      {|
        let f x = (x + 2) * 2 ;;
        let rez = f 2 ;;
      |};
    [%expect 
      {| 
      val f : int -> int = <fun> 
      val rez : int = 8 |}]
  ;;

  let%expect_test _ = 
    interpret_test 
      {|
        let rec fac n = 
          if n < 1 then 1 else n * fac (n - 1) ;;
        let rez = fac 10 ;;
      |};
    [%expect 
      {| 
      val fac : int -> int = <fun>        
      val rez : int = 3628800 |}]
  ;;

    let%expect_test _ =
      interpret_test
        {|
          let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);;
          let init f n =
            let rec helper i last f k =
              if i = last then k []
              else helper (i + 1) last f (fun c -> k (f i :: c))
            in
            helper 0 n f (fun x -> x)
          ;;
          let first_n_fibs n = init (fun i -> fib i) n;;
          let l = first_n_fibs 7;;
        |};
      [%expect 
        {| 
          val fib : int -> int = <fun> 
          val first_n_fibs : int -> int list = <fun> 
          val init : (int -> 'v) -> int -> 'v list = <fun> 
          val l : int list = [1; 1; 2; 3; 5; 8; 13] 
        |}]
  ;;

  let%expect_test _ = 
    interpret_test 
      {|
        let [x; y; z] = [0; 1; 2; 3] ;;
      |};
    [%expect 
      {| Infer error: Expression contains not implemented features |}]
  ;;

  let%expect_test _ = 
    interpret_test 
      {|
        let div_by_zero a b = a / b ;;
        let rez = div_by_zero 1 0 ;;
      |};
    [%expect 
      {| Interpreter error: Division by zero |}]
  ;;

  let%expect_test _ = 
    interpret_test 
      {|
        let rec zip_sum_all lst init =
          match lst with
          | (l, r) :: tl -> zip_sum_all tl l + r + init
          | [] -> init ;; 
    
        let rez = zip_sum_all [1, 2; 3, 4; 5, 6] 0 ;;
      |};
    [%expect 
      {| 
        val rez : int = 21 
        val zip_sum_all : (int * int) list -> int -> int = <fun> 
      |}]
  ;;
end