type bin_op = 
    | Addition
    | Substraction
    | Multiplication
    | Division
    | Equality
    | Concatenation

type expr = 
    | IfStmt of expr * expr * expr
    | LetStmt of string * expr * expr
    | Tuple of expr list
    | List of expr list
    | BinOp of expr * bin_op * expr
    | UnaryNeg of expr
    | IntegerLiteral of int 
    | StringLiteral of string 
    | Identifier of string

exception UnexpectedToken
exception ExpectedToken of Lexer.token

let force_consume l m = match l with
| h::t when m = h -> t
| _ -> raise (ExpectedToken m)

let rec parse_expr tokens = parse_stmt tokens

and parse_stmt tokens = match tokens with
| Lexer.TT_If::tokens -> begin
    let cond, tokens = parse_expr tokens in
    let tokens = force_consume tokens Lexer.TT_Then in
    let e1, tokens = parse_expr tokens in
    let tokens = force_consume tokens Lexer.TT_Else in
    let e2, tokens = parse_expr tokens in
    IfStmt (cond, e1, e2), tokens
end
| Lexer.TT_Let::tokens -> begin
    match tokens with
    | (Lexer.TT_Identifier s)::Lexer.TT_Equals::tokens ->
            let e1, tokens = parse_expr tokens in
            let e2, tokens = parse_expr tokens in
            LetStmt (s, e1, e2), tokens
    | (Lexer.TT_Identifier _)::_::_ -> raise (ExpectedToken Lexer.TT_Equals)
    | _ -> raise (ExpectedToken (Lexer.TT_Identifier "any"))
end
| _ -> begin
    let e, tokens = parse_eq_elem tokens in
    match tokens with
    | Lexer.TT_Equals::tokens -> 
            let expr, tokens = parse_stmt tokens in
            BinOp (e, Equality, expr), tokens
    | _ -> e, tokens
end

and parse_eq_elem tokens = 
    let elem, tokens = parse_tup_elem tokens in
    match tokens with 
    | Lexer.TT_Comma::tokens -> begin
        let e, tokens = parse_eq_elem tokens in
        match e with
        | Tuple l -> Tuple (elem::l), tokens
        | _ -> Tuple ([elem; e]), tokens
    end
    | _ -> elem, tokens

and parse_tup_elem tokens = 
    let term, tokens = parse_term tokens in
    match tokens with
    | Lexer.TT_Plus::tokens ->
            let elem, tokens = parse_tup_elem tokens in
            BinOp (term, Addition, elem), tokens
    | Lexer.TT_Minus::tokens ->
            let elem, tokens = parse_tup_elem tokens in
            BinOp (term, Substraction, elem), tokens
    | Lexer.TT_PPlus::tokens -> 
            let elem, tokens = parse_tup_elem tokens in
            BinOp (term, Concatenation, elem), tokens
    | _ -> term, tokens

and parse_term tokens = 
    let factor, tokens = parse_factor tokens in
    match tokens with
    | Lexer.TT_Asterisk::tokens ->
            let term, tokens = parse_term tokens in
            BinOp (factor, Multiplication, term), tokens
    | Lexer.TT_Slash::tokens ->
            let term, tokens = parse_term tokens in
            BinOp (factor, Division, term), tokens
    | _ -> factor, tokens

and parse_factor tokens = 
    match tokens with
    | Lexer.TT_Minus::tokens -> 
            let atom, tokens = parse_atom tokens in
            UnaryNeg atom, tokens
    | _ -> parse_atom tokens

and parse_atom tokens =
    match tokens with
    | Lexer.TT_LParen::tokens ->
            let e, tokens = parse_expr tokens in
            let tokens = force_consume tokens Lexer.TT_RParen in
            e, tokens
    | Lexer.TT_LBracket::tokens ->
            let l, tokens = parse_list tokens in
            l, tokens
    | (Lexer.TT_Integer n)::tokens -> IntegerLiteral n, tokens
    | (Lexer.TT_String s)::tokens -> StringLiteral s, tokens
    | (Lexer.TT_Identifier s)::tokens -> Identifier s, tokens
    | _ -> raise UnexpectedToken

(* assumes the opening [ has already been consumed *)
and parse_list tokens =
    match tokens with
    | Lexer.TT_RBracket::tokens -> List [], tokens
    | _ -> begin
        let e, tokens = parse_expr tokens in
        let l, tokens = parse_list tokens in
        match l with 
        | List l -> List (e::l), tokens
        | _ -> failwith "unreachable"
    end

let parse tokens =
    let e, tokens = parse_expr tokens in
    match tokens with
    | [] -> e
    | _ -> raise UnexpectedToken
