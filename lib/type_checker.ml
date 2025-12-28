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
| FuncType of (types list) * types | Var of type_variable

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
| TupleType (tl) -> Printf.sprintf "(%s)" (tuple_elems_to_string tl)
| FuncType (tl, r) -> 
        Printf.sprintf "(%s) -> %s" (fct_params_to_string tl) (type_to_string r)
| Var tv -> type_var_to_string tv

and tuple_elems_to_string tl = match tl with
| [] -> ""
| h::[] -> type_to_string h
| h::t -> 
        let s = type_to_string h in
        Printf.sprintf "%s * %s" s (tuple_elems_to_string t)

and fct_params_to_string tl = match tl with
| [] -> ""
| h::[] -> type_to_string h
| h::t -> 
        let s = type_to_string h in
        Printf.sprintf "%s -> %s" s (fct_params_to_string t)

module StringMap = Map.Make(String)

let rec analyse e env = match e with
| IntegerLiteral _ -> IntType
| StringLiteral _ -> StrType
| BoolLiteral _ -> BoolType
| Identifier x -> get_type x env
| Seq (e1, e2) ->
        let t1 = analyse e1 env in
        unify t1 (TupleType []);
        analyse e2 env
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
| FnDecl (v, params, body, expr) ->
        let arg_env, arg_types = List.fold_left (
            fun (env, arg_types) arg ->
                let new_t = new_type_var () in
                StringMap.add arg new_t env, new_t::arg_types) (env, []) params in
        let r = analyse body arg_env in
        let env = StringMap.add v (FuncType (List.rev arg_types, r)) env in
        analyse expr env
| AnonFn (params, body) -> 
        let arg_env, arg_types = List.fold_left (
            fun (env, arg_types) arg ->
                let new_t = new_type_var () in
                StringMap.add arg new_t env, new_t::arg_types) (env, []) params in
        let r = analyse body arg_env in
        FuncType (List.rev arg_types, r)
| Call (f, args) ->
        let arg_types = List.map (fun arg -> analyse arg env) args in
        let f_type = analyse f env in
        let r_type = new_type_var () in
        unify (FuncType (arg_types, r_type)) f_type;
        r_type
| UnaryNeg e' -> 
        let inner_type = analyse e' env in
        unify inner_type IntType;
        IntType
| UnaryNot e' ->
        let inner_type = analyse e' env in
        unify inner_type BoolType;
        BoolType
| BinOp (e1, LogicOr, e2)
| BinOp (e1, LogicAnd, e2) ->
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 t2;
        BoolType
| BinOp (e1, Eq, e2) 
| BinOp (e1, Neq, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 t2;
        BoolType
| BinOp (e1, Lt, e2) 
| BinOp (e1, Leq, e2)
| BinOp (e1, Gt, e2)
| BinOp (e1, Geq, e2) ->
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 IntType;
        unify t2 IntType;
        BoolType
| BinOp (e1, Concat, e2) ->
        let t1 = analyse e1 env in 
        let t2 = analyse e2 env in
        unify t1 StrType;
        unify t2 StrType;
        StrType
| BinOp (e1, Add, e2) 
| BinOp (e1, Sub, e2) 
| BinOp (e1, Mul, e2)
| BinOp (e1, Div, e2) -> 
        let t1 = analyse e1 env in
        let t2 = analyse e2 env in
        unify t1 IntType;
        unify t2 IntType;
        IntType

(* TODO: rewrite with function with just one argument *)
| BinOp(e1, Comp, e2) -> begin
    let t1 = analyse e1 env in
    let t2 = analyse e2 env in
    match t2 with
    | FuncType ([], r) ->
            unify t1 (TupleType []);
            FuncType ([], r)
    | FuncType (arg::[], r) ->
            unify t1 arg;
            FuncType ([], r)
    | FuncType (arg::tail, r) ->
            unify t1 arg;
            FuncType (tail, r)
    | _ -> 
            let any1 = new_type_var () in
            let any2 = new_type_var () in
            raise (IncompatibleTypes (t2, FuncType ([t1; any1], any2)))
end
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
    | FuncType (tl1, r1), FuncType (tl2, r2) ->
            unify (TupleType tl1) (TupleType tl2);
            unify r1 r2
    | _ when a = b -> ()
    | _ -> raise (IncompatibleTypes (t1, t2))
