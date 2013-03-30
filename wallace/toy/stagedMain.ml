open StagedCommon
open StagedTranslate
open StagedEval
open StagedPrint
open StagedToToy

let inferty env exp =
  try
    let ty = StagedTypes.typ [0] env exp in
    print_endline "inferred type:";
    Format.print_flush ();
    print_ty ty;
    Format.print_flush ();
    print_endline "";
    Some ty
  with exc ->
    print_endline ("exception in type inference: " ^ Printexc.to_string exc);
    None

let eval fn exp msg printer err_msg =
  try
    let v = fn exp in
    print_endline msg;
    Format.print_flush ();
    printer v;
    Format.print_flush ();
    print_endline "";
    Some v
  with exc ->
    print_endline (err_msg ^ Printexc.to_string exc);
    None

let run repl lexbuf =
  let open StagedTypes in
  let rec iter (staged_env : value env)
               (staged_scmenv : StagedTypes.tyenv)
               (record_env : value env)
               (record_scmenv : StagedTypes.tyenv)
               toy_env
               (translate_env : id list) =
    if repl then
      print_string "> ";
      flush stdout;

      try
        match StagedParser.main StagedLexer.mytoken lexbuf with
        | Decl (Valbind (id, exp)) ->
            let is_expansive = expansive 0 exp in

            let staged_ty = inferty [staged_scmenv] exp in
            let staged_scmenv' =
              match staged_ty with
              | None -> staged_scmenv
              | Some ty ->
                  let scm =
                    if is_expansive then
                      Scheme ([], FieldType ty)
                    else
                      generalize 0 (FieldType ty)
                  in
                  Row (id, scm, staged_scmenv)
            in

            let staged_val = eval
              (StagedEval.runWEnv staged_env)
              exp
              "evaluated with StagedEval:"
              print_value
              "error while running staged calc: "
            in
            let staged_env' =
              match staged_val with
              | None -> staged_env
              | Some v -> (id, ref v) :: staged_env
            in

            let (translation, _) = translate translate_env exp in

            let record_ty = inferty [record_scmenv] translation in
            let record_scmenv' =
              match record_ty with
              | None -> record_scmenv
              | Some ty ->
                  let scm =
                    if is_expansive then
                      Scheme ([], FieldType ty)
                    else
                      generalize 0 (FieldType ty)
                in
                Row (id, scm, record_scmenv)
            in

            let record_val = eval
              (RecordEval.runWEnv record_env)
              translation
              "evaluated with RecordEval:"
              print_value
              "error while running record calc: "
            in
            let record_env' = match record_val with
            | None -> record_env
            | Some e -> (id, ref e) :: record_env
            in

            Printf.printf "translated expression in toy:\n";
            let toy_env' =
              let trans_exp =
                if is_expansive then
                  AppE (AbsE (Abs ("__p__", IdE "__p__")), translation)
                else
                  translation
              in
              match Toy.handle_phrase toy_env (ToySyntax.PhraseExpr (stagedToToy trans_exp)) with
              | (scheme, None) -> Toy.Engine.add_to_env id scheme toy_env
              | (_, Some e) -> e
            in

            iter staged_env'
                 staged_scmenv'
                 record_env'
                 record_scmenv'
                 toy_env'
                 (List.map fst staged_env')

        | Exp exp ->
            let _ = inferty [staged_scmenv] exp in
            let _ = eval
              (StagedEval.runWEnv staged_env)
              exp
              "evaluated with StagedEval:"
              print_value
              "error while running staged calc: "
            in

            let (translation, _) = translate translate_env exp in

            let _ = inferty [record_scmenv] translation in

            let _ = eval
              (RecordEval.runWEnv record_env)
              translation
              "evaluated with RecordEval:"
              print_value
              "error while running record calc: "
            in

            Printf.printf "translated expression in toy:\n";
            let _ = Toy.handle_phrase toy_env (ToySyntax.PhraseExpr (stagedToToy translation)) in

            iter staged_env
                 staged_scmenv
                 record_env
                 record_scmenv
                 toy_env
                 translate_env

      with Parsing.Parse_error -> print_endline "Parse error."
         | StagedLexer.EndInput -> ()
         | exc -> print_endline (Printexc.to_string exc)
  in

  iter StagedCommon.stdenv
       StagedTypes.stdenv_tyrec
       StagedCommon.stdenv
       StagedTypes.stdenv_tyrec
       Toy.Engine.builtin
       (List.map fst StagedCommon.stdenv)

let _ =
  if Array.length (Sys.argv) < 2 then
    let lexbuf = Lexing.from_channel stdin in
    run true lexbuf
  else
    let input_file = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in input_file) in
    run false lexbuf
