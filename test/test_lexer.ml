open Typfun

let test_lexer_basic s exp = 
    let res = Lexer.lex_line s in
    if exp <> res then
        failwith s


let test () =
    Printf.printf "testing lexer...\n";
    test_lexer_basic "+" [ Lexer.TT_Plus ];
    test_lexer_basic "++" [ Lexer.TT_Plus_Plus ];
    test_lexer_basic "/" [ Lexer.TT_Slash ];
    test_lexer_basic "* -- }" [ 
        Lexer.TT_Star; Lexer.TT_Minus; Lexer.TT_Minus; Lexer.TT_RBrace ];
    test_lexer_basic "12 1234" [ Lexer.TT_Int 12; Lexer.TT_Int 1234];
    test_lexer_basic "foo bar" [ Lexer.TT_Ident "foo"; Lexer.TT_Ident "bar" ];
    test_lexer_basic "if = else" [ Lexer.TT_If; Lexer.TT_Eq; Lexer.TT_Else]
