open Typfun
open Typfun.Type_checker

let empty_env = StringMap.empty

let test_success e exp env =
    let t = analyse e env in
    (* Printf.printf "%s\n" (type_to_string t); *)
    assert (t = exp)

let test_error e exp env =
    try 
        let _ = analyse e env in
        failwith "expected error got succes"
    with 
    | err when err = exp -> ()
    | err -> raise err

let test () =
    Printf.printf "testing type checker...\n";

    (* basic tests *)
    test_success (Parser.BinOp (
        Parser.IntegerLiteral 1, 
        Parser.Addition, 
        Parser.IntegerLiteral 2)) IntType empty_env;

    test_error (Parser.Identifier "a") (Undefined_identifier "a") empty_env;

    (* simple inference test *)
    let a_type = new_type_var () in
    let b_type = new_type_var () in
    let env = StringMap.add "a" a_type empty_env in
    let env = StringMap.add "b" b_type env in
    (* infers that a_type = b_type *)
    test_success (Parser.BinOp (
        Parser.Identifier "a",
        Parser.Equality,
        Parser.Identifier "b"
    )) BoolType env;
    (* infers that a_type = int *)
    test_success (Parser.BinOp (
        Parser.Identifier "a",
        Parser.Addition,
        Parser.IntegerLiteral 1
    )) IntType env;
    (* so b_type must also be int *)
    test_success (Parser.Identifier "b") IntType env;

    (* tuple test *)
    let a_type = new_type_var () in
    let b_type = new_type_var () in
    let c_type = new_type_var () in
    let d_type = new_type_var () in
    let env = StringMap.add "a" a_type empty_env in
    let env = StringMap.add "b" b_type env in
    let env = StringMap.add "c" c_type env in
    let env = StringMap.add "d" d_type env in
    (*  (c, b, a, d) = (b, a, "coucou", 0)  *)
    test_success (Parser.BinOp (
        Parser.Tuple [ 
            Parser.Identifier "c"; 
            Parser.Identifier "b"; 
            Parser.Identifier "a";
            Parser.Identifier "d"
        ], Parser.Equality,
        Parser.Tuple [
            Parser.Identifier "b";
            Parser.Identifier "a";
            Parser.StringLiteral "coucou";
            Parser.IntegerLiteral 0
        ]
    )) BoolType env;
    test_success (Parser.Identifier "a") StrType env;
    test_success (Parser.Identifier "b") StrType env;
    test_success (Parser.Identifier "c") StrType env;
    test_success (Parser.Identifier "d") IntType env;

    let a_type = new_type_var () in
    let env = StringMap.add "a" a_type empty_env in
    test_error (Parser.BinOp (
        Parser.Identifier "a",
        Parser.Equality,
        Parser.Tuple [
            Parser.IntegerLiteral 1;
            Parser.Identifier "a"
        ]
    )) Recursive_unification env;

    (* if test *)
    test_success (Parser.IfStmt (
        Parser.BinOp (
            Parser.IntegerLiteral 1,
            Parser.Equality,
            Parser.IntegerLiteral 2
        ),
        Parser.StringLiteral "if body",
        Parser.StringLiteral "else body"
    )) StrType empty_env;

    test_error (Parser.IfStmt (
        Parser.StringLiteral "not a bool",
        Parser.IntegerLiteral 1,
        Parser.IntegerLiteral 2
    )) (IncompatibleTypes (StrType, BoolType)) empty_env;

    test_error (Parser.IfStmt (
        Parser.BinOp (
            Parser.IntegerLiteral 1, 
            Parser.Equality, 
            Parser.IntegerLiteral 2),
        Parser.StringLiteral "not an int",
        Parser.IntegerLiteral 0
    )) (IncompatibleTypes (StrType, IntType)) empty_env;

    (* test let *)
    test_success (Parser.LetStmt (
        "a", Parser.IntegerLiteral 1, Parser.Identifier "a")) IntType empty_env;
    test_error (Parser.LetStmt (
        "a", Parser.Identifier "a", Parser.StringLiteral "coucou"
    )) (Undefined_identifier "a") empty_env


