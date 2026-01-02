open Typfun

let () =
    Printf.printf "Welcome to the Typfun typing repl!\n\n";
    let ctx = Type_checker.init_ctx () in
    while true do
        Printf.printf ">>> ";
        let s = read_line () in
        let t = Lexer.lex_line s in
        let ast = Parser.parse t in
        let typ = Type_checker.analyse ast empty_env ctx in
        
        let str_ctx = Type_checker.init_ctx () in
        Printf.printf ": %s\n" (Type_checker.type_to_string typ str_ctx)
    done
