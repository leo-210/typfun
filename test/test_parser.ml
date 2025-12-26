open Typfun

let test_success t exp =
    let e = Parser.parse t in
    assert (e = exp)

let test_error t err =
    try 
        let _ = Parser.parse t in
        failwith "expected error, got success"
    with 
    | e when e = err -> ()
    | e -> raise e

let test () =
    Printf.printf "testing parser...\n";
    test_success [ Lexer.TT_Int 2; Lexer.TT_Plus; Lexer.TT_Int 3 ] (
        Parser.BinOp (
            Parser.IntegerLiteral 2, Parser.Add, Parser.IntegerLiteral 3));
    test_success [ Lexer.TT_Minus; Lexer.TT_Int 1 ] (
        Parser.UnaryNeg (Parser.IntegerLiteral 1));
    test_success [ Lexer.TT_Int 1; Lexer.TT_Plus; Lexer.TT_Int 2; 
        Lexer.TT_Comma; Lexer.TT_Str "coucou" ] (
        Parser.Tuple [ 
            BinOp (
                Parser.IntegerLiteral 1, Parser.Add, Parser.IntegerLiteral 2);
            Parser.StringLiteral "coucou" ]);
    test_success [ Lexer.TT_Int 1; Lexer.TT_Eq_Eq; Lexer.TT_Int 1; 
            Lexer.TT_Plus; Lexer.TT_Int 2] (Parser.BinOp (
                Parser.IntegerLiteral 1, Parser.Eq, Parser.BinOp (
                    Parser.IntegerLiteral 1, 
                    Parser.Add, 
                    Parser.IntegerLiteral 2)));
    test_success [ Lexer.TT_If; Lexer.TT_Int 1; Lexer.TT_Comma; 
        Lexer.TT_Int 4; Lexer.TT_Then; Lexer.TT_Int 2; Lexer.TT_Else; 
        Lexer.TT_Int 3] (Parser.IfStmt (
            Parser.Tuple [IntegerLiteral 1; IntegerLiteral 4],
            Parser.IntegerLiteral 2,
            Parser.IntegerLiteral 3));
    test_error [ Lexer.TT_Int 1; Lexer.TT_Int 2 ] Parser.UnexpectedToken;
    test_error [ Lexer.TT_LParen; Lexer.TT_Int 1 ] (
        Parser.ExpectedToken Lexer.TT_RParen);
    test_error [ Lexer.TT_If; Lexer.TT_Int 1; Lexer.TT_Ident "foo" ] (
        Parser.ExpectedToken Lexer.TT_Then)
