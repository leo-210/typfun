type bin_op = 
    | LogicOr
    | LogicAnd
    | Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq
    | Add
    | Sub
    | Mul
    | Div
    | Concat
    | Comp

type expr = 
    | Seq of expr * expr
    | IfStmt of expr * expr * expr
    | LetStmt of string * expr * expr
    | FnDecl of string * (string list) * expr * expr
    | AnonFn of (string list) * expr
    | Call of expr * (expr list)
    | Tuple of expr list
    | BinOp of expr * bin_op * expr
    | UnaryNot of expr
    | UnaryNeg of expr
    | IntegerLiteral of int 
    | StringLiteral of string 
    | BoolLiteral of bool
    | Identifier of string

exception UnexpectedToken
exception ExpectedToken of Lexer.token

type ctx = {
    in_args : bool
}

let force_consume l m = match l with
| h::t when m = h -> t
| _ -> raise (ExpectedToken m)

let rec parse_expr tokens ctx = match tokens with 
| Lexer.TT_LBrace::tokens ->
        let expr, tokens = parse_expr tokens { in_args = false } in
        let tokens = force_consume tokens Lexer.TT_RBrace in
        AnonFn ([], expr), tokens
| _ -> begin
    let stmt, tokens = parse_stmt tokens ctx in
    match tokens with 
    | Lexer.TT_Semicolon::tokens -> 
            let e, tokens = parse_expr tokens ctx in
            Seq (stmt, e), tokens
    | _ -> stmt, tokens
end

(* TODO *)
and parse_params tokens = [], tokens

and parse_stmt tokens ctx = match tokens with
| Lexer.TT_If::tokens -> begin
    let cond, tokens = parse_expr tokens { in_args = false } in
    let tokens = force_consume tokens Lexer.TT_Then in
    let e1, tokens = parse_expr tokens { in_args = false } in
    let tokens = force_consume tokens Lexer.TT_Else in
    let e2, tokens = parse_expr tokens ctx in
    IfStmt (cond, e1, e2), tokens
end
| Lexer.TT_Let::tokens -> begin
    match tokens with
    | (Lexer.TT_Ident s)::Lexer.TT_Eq::tokens ->
            let e1, tokens = parse_expr tokens { in_args = false } in
            let tokens = force_consume tokens Lexer.TT_In in
            let e2, tokens = parse_expr tokens ctx in
            LetStmt (s, e1, e2), tokens
    | (Lexer.TT_Ident _)::_::_ -> raise (ExpectedToken Lexer.TT_Eq)
    | _ -> raise (ExpectedToken (Lexer.TT_Ident ""))
end
| Lexer.TT_Fn::(Lexer.TT_Ident v)::Lexer.TT_LParen::tokens -> 
        let params, tokens = parse_params tokens in
        let tokens = force_consume tokens Lexer.TT_RParen in
        let tokens = force_consume tokens Lexer.TT_LBrace in
        let body, tokens = parse_expr tokens { in_args = false } in
        let tokens = force_consume tokens Lexer.TT_RBrace in
        let e, tokens = parse_expr tokens ctx in
        FnDecl (v, params, body, e), tokens

| Lexer.TT_Fn::Lexer.TT_LParen::tokens -> 
        let params, tokens = parse_params tokens in
        let tokens = force_consume tokens Lexer.TT_RParen in
        let tokens = force_consume tokens Lexer.TT_LBrace in
        let body, tokens = parse_expr tokens { in_args = false } in
        let tokens = force_consume tokens Lexer.TT_RBrace in
        AnonFn (params, body), tokens
| Lexer.TT_Fn::_ -> raise (ExpectedToken Lexer.TT_LParen)
| _ -> parse_logic_or tokens ctx

and parse_logic_or tokens ctx = 
    let e1, tokens = parse_logic_and tokens ctx in
    match tokens with
    | Lexer.TT_Or::tokens ->
            let e2, tokens = parse_logic_or tokens ctx in
            BinOp (e1, LogicOr, e2), tokens
    | _ -> e1, tokens

and parse_logic_and tokens ctx =
    let e1, tokens = parse_equality tokens ctx in
    match tokens with
    | Lexer.TT_And::tokens ->
            let e2, tokens = parse_logic_and tokens ctx in
            BinOp (e1, LogicAnd, e2), tokens
    | _ -> e1, tokens

and parse_equality tokens ctx = 
    let e1, tokens = parse_comparison tokens ctx in
    match tokens with 
    | Lexer.TT_Eq_Eq::tokens -> 
            let e2, tokens = parse_equality tokens ctx in
            BinOp (e1, Eq, e2), tokens
    | Lexer.TT_Bang_Eq::tokens ->
            let e2, tokens = parse_equality tokens ctx in
            BinOp (e1, Neq, e2), tokens
    | _ -> e1, tokens

and parse_comparison tokens ctx =
    let e1, tokens = 
        if ctx.in_args then
            parse_term tokens ctx
        else
            parse_tuple tokens
    in match tokens with
    | Lexer.TT_Lt::tokens ->
            let e2, tokens = parse_comparison tokens ctx in
            BinOp (e1, Lt, e2), tokens
    | Lexer.TT_Lt_Eq::tokens ->
            let e2, tokens = parse_comparison tokens ctx in
            BinOp (e1, Leq, e2), tokens
    | Lexer.TT_Gt::tokens ->
            let e2, tokens = parse_comparison tokens ctx in
            BinOp (e1, Gt, e2), tokens
    | Lexer.TT_Gt_Eq::tokens ->
            let e2, tokens = parse_comparison tokens ctx in
            BinOp (e1, Geq, e2), tokens
    | _ -> e1, tokens

and parse_tuple tokens =
    let head, tokens = parse_term tokens { in_args = false } in
    match tokens with 
    | Lexer.TT_Comma::tokens -> begin
        let tail, tokens = parse_tuple tokens in
        match tail with
        | Tuple l -> Tuple (head::l), tokens
        | _ -> Tuple ([head; tail]), tokens
    end
    | _ -> head, tokens

and parse_term tokens ctx = 
    let term, tokens = parse_factor tokens ctx in
    match tokens with
    | Lexer.TT_Plus::tokens ->
            let elem, tokens = parse_term tokens ctx in
            BinOp (term, Add, elem), tokens
    | Lexer.TT_Minus::tokens ->
            let elem, tokens = parse_term tokens ctx in
            BinOp (term, Sub, elem), tokens
    | Lexer.TT_Plus_Plus::tokens -> 
            let elem, tokens = parse_term tokens ctx in
            BinOp (term, Concat, elem), tokens
    | _ -> term, tokens

and parse_factor tokens ctx = 
    let e1, tokens = parse_unary tokens ctx in
    match tokens with
    | Lexer.TT_Star::tokens ->
            let e2, tokens = parse_factor tokens ctx in
            BinOp (e1, Mul, e2), tokens
    | Lexer.TT_Slash::tokens ->
            let e2, tokens = parse_factor tokens ctx in
            BinOp (e1, Div, e2), tokens
    | _ -> e1, tokens

and parse_unary tokens ctx = 
    match tokens with
    | Lexer.TT_Minus::tokens -> 
            let e, tokens = parse_unary tokens ctx in
            UnaryNeg e, tokens
    | Lexer.TT_Bang::tokens ->
            let e, tokens = parse_unary tokens ctx in
            UnaryNot e, tokens
    | _ -> parse_call tokens ctx

and parse_call tokens ctx =
    let c, tokens = parse_composition tokens ctx in
    match tokens with
    | Lexer.TT_LParen::Lexer.TT_RParen::tokens -> Call (c, []), tokens
    | Lexer.TT_LParen::tokens ->
            let args, tokens = parse_args tokens in
            let tokens = force_consume tokens Lexer.TT_RParen in
            Call (c, args), tokens
    | _ -> c, tokens

(* TODO *)
and parse_args tokens = [], tokens

and parse_composition tokens ctx =
    let e1, tokens = parse_atom tokens in
    match tokens with
    | Lexer.TT_Dot::tokens ->
            let e2, tokens = parse_composition tokens ctx in
            BinOp (e1, Comp, e2),tokens
    | _ -> e1, tokens

and parse_atom tokens =
    match tokens with
    | Lexer.TT_LParen::Lexer.TT_RParen::tokens -> Tuple [], tokens
    | Lexer.TT_LParen::tokens ->
            let e, tokens = parse_expr tokens { in_args = false } in
            let tokens = force_consume tokens Lexer.TT_RParen in
            e, tokens
    | (Lexer.TT_Int n)::tokens -> IntegerLiteral n, tokens
    | Lexer.TT_False::tokens -> BoolLiteral false, tokens
    | Lexer.TT_True::tokens -> BoolLiteral true, tokens
    | (Lexer.TT_Str s)::tokens -> StringLiteral s, tokens
    | (Lexer.TT_Ident s)::tokens -> Identifier s, tokens
    | _ -> raise UnexpectedToken

let parse tokens =
    let e, tokens = parse_expr tokens { in_args = false } in
    match tokens with
    | [] -> e
    | _ -> raise UnexpectedToken
