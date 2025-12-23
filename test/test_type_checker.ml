open Typfun

let test_success e exp =
    let t = Type_checker.type_check e in
    assert (t = exp)

let test () =
    Printf.printf "testing type checker...\n";
    test_success (Parser.BinOp (
        Parser.IntegerLiteral 1, 
        Parser.Addition, 
        Parser.IntegerLiteral 2)) Type_checker.IntegerType;
    test_success (Parser.BinOp (
        Parser.Identifier "a",
        Parser.Addition,
        Parser.Identifier "b"
    )) Type_checker.IntegerType;
    test_success (Parser.Tuple [Parser.Identifier "a"; Parser.BinOp (
        Parser.Identifier "a",
        Parser.Addition,
        Parser.IntegerLiteral 1
    )]) (Type_checker.TupleType ([Type_checker.IntegerType; Type_checker.IntegerType], 2))
