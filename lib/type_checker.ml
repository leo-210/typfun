open Parser

type context = {
    mutable next_name : char;
    mutable next_id : int
}

let ctx = {
    next_name = 'a';
    next_id = 0;
}

type type_variable = {
    id: int;
    mutable name: string;
    mutable instance : types option;
}
and types = IntType | StrType | BoolType | TupleType of (types list) 
| Var of type_variable

let rec new_type_var () = 
    ctx.next_id <- ctx.next_id + 1;
    {
        id = ctx.next_id - 1;
        name = "";
        instance = None
    }

and type_var_name t =
    if t.name = "" then begin
        t.name <- Char.escaped ctx.next_name;
        ctx.next_name <- Char.chr ((Char.code ctx.next_name) - 1)
    end;
    t.name

and type_var_to_string t = match t.instance with
| None -> type_var_name t
| Some t' -> type_to_string t'

and type_to_string t = match t with
| IntType -> "int"
| StrType -> "str"
| BoolType -> "bool"
| TupleType [] -> ""
| TupleType (h::[]) -> Printf.sprintf "%s" (type_to_string h)
| TupleType (h::t) -> 
        Printf.sprintf "%s * %s" (type_to_string h) (
            type_to_string (TupleType t))
| Var tv -> type_var_to_string tv

module StringMap = Map.Make(String)

let rec analyse e env = match e with
| IntegerLiteral _ -> IntType
| StringLiteral _ -> StrType
| Identifier x -> get_type x env
| IfStmt (cond, if_body, else_body) ->
        let cond_type = analyse cond env in
        unify cond_type BoolType env;
        let if_type = analyse if_body env in
        let else_type = analyse else_body env in
        unify if_type else_type env;
        if_type
| LetStmt (v, def, body) ->
        let def_type = analyse def env in
        let new_env = StringMap.add v def_type env in
        analyse body new_env
| UnaryNeg e' -> 
        let inner_type = analyse e' env in
        unify inner_type IntType env;
        IntType
| BinOp (e1, Equality, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 t2 env;
        BoolType
| BinOp (e1, Concatenation, e2) ->
        let t1 = analyse e1 env in 
        let t2 = analyse e2 env in
        unify t1 StrType env;
        unify t2 StrType env;
        StrType
| BinOp (e1, Addition, e2) 
| BinOp (e1, Substraction, e2) 
| BinOp (e1, Multiplication, e2)
| BinOp (e1, Division, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 IntType env;
        unify t2 IntType env;
        IntType
| Tuple [] -> TupleType []
| Tuple (h::t) -> begin
    let h_type = analyse h env in
    let t_type = analyse (Tuple t) env in
    match t_type with
    | TupleType tl -> TupleType (h_type::tl)
    | _ -> failwith "unreachable"
end

(* TODO *)
and unify t1 t2 env = ()
and get_type t env = IntType
