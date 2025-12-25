open Parser

type context = {
    mutable next_id: int;
    mutable next_name: char;
}

let ctx = {
    next_id = 0;
    next_name = 'a'
}

type type_variable = {
    id: int;
    mutable name: string;
    mutable instance: types option;
}
and types = IntType | StrType | BoolType | TupleType of (types list) 
| Var of type_variable

exception Undefined_identifier of string
exception Recursive_unification
exception IncompatibleTypes of types * types

let new_type_var () = 
    ctx.next_id <- ctx.next_id + 1;
    Var {
        id = ctx.next_id - 1;
        name = "";
        instance = None
    }

let rec type_var_name t =
    if t.name = "" then begin
        t.name <- Char.escaped ctx.next_name;
        ctx.next_name <- Char.chr ((Char.code ctx.next_name) + 1)
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
        let s = type_to_string h in
        Printf.sprintf "%s * %s" s (type_to_string (TupleType t))
| Var tv -> type_var_to_string tv

module StringMap = Map.Make(String)

let rec analyse e env = match e with
| IntegerLiteral _ -> IntType
| StringLiteral _ -> StrType
| Identifier x -> get_type x env
| IfStmt (cond, if_body, else_body) ->
        let cond_type = analyse cond env in
        unify cond_type BoolType;
        let if_type = analyse if_body env in
        let else_type = analyse else_body env in
        unify if_type else_type;
        if_type
| LetStmt (v, def, body) ->
        let def_type = analyse def env in
        let new_env = StringMap.add v def_type env in
        analyse body new_env
| UnaryNeg e' -> 
        let inner_type = analyse e' env in
        unify inner_type IntType;
        IntType
| BinOp (e1, Equality, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 t2;
        BoolType
| BinOp (e1, Concatenation, e2) ->
        let t1 = analyse e1 env in 
        let t2 = analyse e2 env in
        unify t1 StrType;
        unify t2 StrType;
        StrType
| BinOp (e1, Addition, e2) 
| BinOp (e1, Substraction, e2) 
| BinOp (e1, Multiplication, e2)
| BinOp (e1, Division, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 IntType;
        unify t2 IntType;
        IntType
| Tuple [] -> TupleType []
| Tuple (h::t) -> begin
    let h_type = analyse h env in
    let t_type = analyse (Tuple t) env in
    match t_type with
    | TupleType tl -> TupleType (h_type::tl)
    | _ -> failwith "unreachable"
end

and prune t = match t with
| Var tv -> begin
    match tv.instance with
    | Some i ->
        let t' = prune i in
        tv.instance <- Some t';
        t'
    | None -> t
end
| _ -> t

and get_type v env = match StringMap.find_opt v env with
| Some t -> prune t
| None -> raise (Undefined_identifier v) 

and occurs_in_type t1 t2 =
    let t2 = prune t2 in
    match t2 with
    | Var t when t1 = t -> true
    | TupleType tl -> List.exists (fun x -> occurs_in_type t1 x) tl
    | _ -> false

and unify t1 t2 : unit =
    let a, b = prune t1, prune t2 in
    match a, b with
    | Var tva, Var tvb -> 
            if tva.id <> tvb.id then begin
                if occurs_in_type tva b then
                    raise Recursive_unification;
                tva.instance <- Some b
            end
    | Var tv, _ ->
            if occurs_in_type tv b then
                raise Recursive_unification;
            tv.instance <- Some b
    | _, Var _ -> unify b a
    | TupleType (h1::tl1), TupleType (h2::tl2) ->
            unify h1 h2;
            unify (TupleType tl1) (TupleType tl2)
    | _ when a = b -> ()
    | _ -> raise (IncompatibleTypes (t1, t2))
