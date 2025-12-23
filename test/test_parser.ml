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
    test_success [ Lexer.TT_Integer 2; Lexer.TT_Plus; Lexer.TT_Integer 3 ] (
        Parser.BinOp (
            Parser.IntegerLiteral 2, Parser.Addition, Parser.IntegerLiteral 3));
    test_success [ Lexer.TT_Minus; Lexer.TT_Integer 1 ] (
        Parser.UnaryNeg (Parser.IntegerLiteral 1));
    test_success [ Lexer.TT_Integer 1; Lexer.TT_Plus; Lexer.TT_Integer 2; 
        Lexer.TT_Comma; Lexer.TT_String "coucou" ] (
        Parser.Tuple [ 
            BinOp (
                Parser.IntegerLiteral 1, Parser.Addition, Parser.IntegerLiteral 2);
            Parser.StringLiteral "coucou" ]);
    test_success [ Lexer.TT_LBracket; Lexer.TT_Integer 1; Lexer.TT_RBracket ] (
        Parser.List [ Parser.IntegerLiteral 1 ]);
    test_error [ Lexer.TT_Integer 1; Lexer.TT_Integer 2 ] Parser.UnexpectedToken;
    test_error [ Lexer.TT_LParen; Lexer.TT_Integer 1 ] Parser.MissingClosingParen
