open Parser

type context = {
    mutable next_id: int;
    mutable next_name: char;
}

let init_ctx () = {
    next_id = 0;
    next_name = 'a'
}

type type_variable = {
    id: int;
    mutable name: string;
    mutable instance: types option;
}
and types = IntType | StrType | BoolType | TupleType of (types list) 
| FuncType of types * types | Var of type_variable

exception Undefined_identifier of string
exception Recursive_unification
exception IncompatibleTypes of types * types

let new_type_var ctx = 
    ctx.next_id <- ctx.next_id + 1;
    Var {
        id = ctx.next_id - 1;
        name = "";
        instance = None
    }

let rec prune t = match t with
| Var tv -> begin
    match tv.instance with
    | Some i ->
        let t' = prune i in
        tv.instance <- Some t';
        t'
    | None -> t
end
| _ -> t

let rec type_var_name t ctx =
    if t.name = "" then begin
        t.name <- Char.escaped ctx.next_name;
        ctx.next_name <- Char.chr ((Char.code ctx.next_name) + 1)
    end;
    t.name

and type_var_to_string t ctx = match t.instance with
| None -> type_var_name t ctx
| Some t' -> type_to_string t' ctx

and type_to_string t ctx = match t with
| IntType -> "int"
| StrType -> "str"
| BoolType -> "bool"
| TupleType (tl) -> Printf.sprintf "(%s)" (tuple_elems_to_string tl ctx)
| FuncType (_, _) -> 
        Printf.sprintf "(%s)" (fct_to_string t ctx)
| Var tv -> type_var_to_string tv ctx

and tuple_elems_to_string tl ctx = match tl with
| [] -> ""
| h::[] -> type_to_string h ctx
| h::t -> 
        let s = type_to_string h ctx in
        Printf.sprintf "%s * %s" s (tuple_elems_to_string t ctx)

and fct_to_string t ctx = match t with
| FuncType (a, FuncType (b, r)) ->
        let s = type_to_string a ctx in
        Printf.sprintf "%s -> %s" s (fct_to_string (FuncType (b, r)) ctx)
| FuncType (a, Var t) -> begin
    let t = prune (Var t) in
    match t with
    | FuncType _ -> fct_to_string (FuncType (a, t)) ctx
    | _ -> 
            let s = type_to_string a ctx in
            Printf.sprintf "%s -> %s" s (type_to_string t ctx)
end
| FuncType (a, r) ->
        let s = type_to_string a ctx in
        Printf.sprintf "%s -> %s" s (type_to_string r ctx)
| _ -> failwith "not a function"

module StringMap = Map.Make(String)

let rec aux_analyse (e : expr) (env : types StringMap.t) (ctx : context) = match e with
| IntegerLiteral _ -> IntType
| StringLiteral _ -> StrType
| BoolLiteral _ -> BoolType
| Identifier x -> get_type x env
| Seq (e1, e2) ->
        let t1 = aux_analyse e1 env ctx in
        unify t1 (TupleType []);
        aux_analyse e2 env ctx
| IfStmt (cond, if_body, else_body) ->
        let cond_type = aux_analyse cond env ctx in
        unify cond_type BoolType;
        let if_type = aux_analyse if_body env ctx in
        let else_type = aux_analyse else_body env ctx in
        unify if_type else_type;
        if_type
| LetStmt (v, def, body) ->
        let def_type = aux_analyse def env ctx in
        let new_env = StringMap.add v def_type env in
        aux_analyse body new_env ctx
| FnDecl (v, [], body, expr) ->
        let r_var_t = new_type_var ctx in
        let env = StringMap.add v (FuncType (TupleType [], r_var_t)) env in
        let r = aux_analyse body env ctx in
        unify r_var_t r;
        aux_analyse expr env ctx
| FnDecl (v, params, body, expr) -> 
        let r_type = new_type_var ctx in
        let fct_t, arg_env = List.fold_right (
            fun a (t, env) -> 
                let a_t = new_type_var ctx in
                let env = StringMap.add a a_t env in
                FuncType (a_t, t), env) params (r_type, env) in
        let arg_env = StringMap.add v fct_t arg_env in
        let r_type2 = aux_analyse body arg_env ctx in
        unify r_type r_type2;
        let env = StringMap.add v fct_t env in
        aux_analyse expr env ctx
| AnonFn ([], body) ->
        let r = aux_analyse body env ctx in
        FuncType (TupleType [], r)
| AnonFn (params, body) ->
        let r_type = new_type_var ctx in
        let fct_t, arg_env = List.fold_right (
            fun a (t, env) -> 
                let a_t = new_type_var ctx in
                let env = StringMap.add a a_t env in
                FuncType (a_t, t), env) params (r_type, env) in
        let r_type2 = aux_analyse body arg_env ctx in
        unify r_type r_type2;
        fct_t
| Call (f, args) ->
        let arg_types = List.map (fun arg -> aux_analyse arg env ctx) args in
        let f_type = aux_analyse f env ctx in
        let r_type = new_type_var ctx in
        let infered_t = List.fold_right (fun a acc -> FuncType (a, acc)) arg_types r_type in
        unify infered_t f_type;
        r_type
| UnaryNeg e' -> 
        let inner_type = aux_analyse e' env ctx in
        unify inner_type IntType;
        IntType
| UnaryNot e' ->
        let inner_type = aux_analyse e' env ctx in
        unify inner_type BoolType;
        BoolType
| BinOp (e1, LogicOr, e2)
| BinOp (e1, LogicAnd, e2) ->
        let t1 = aux_analyse e1 env ctx in
        let t2 = aux_analyse e2 env ctx in
        unify t1 t2;
        BoolType
| BinOp (e1, Eq, e2) 
| BinOp (e1, Neq, e2) -> 
        let t1 = aux_analyse e1 env ctx in
        let t2 = aux_analyse e2 env ctx in
        unify t1 t2;
        BoolType
| BinOp (e1, Lt, e2) 
| BinOp (e1, Leq, e2)
| BinOp (e1, Gt, e2)
| BinOp (e1, Geq, e2) ->
        let t1 = aux_analyse e1 env ctx in
        let t2 = aux_analyse e2 env ctx in
        unify t1 IntType;
        unify t2 IntType;
        BoolType
| BinOp (e1, Concat, e2) ->
        let t1 = aux_analyse e1 env ctx in 
        let t2 = aux_analyse e2 env ctx in
        unify t1 StrType;
        unify t2 StrType;
        StrType
| BinOp (e1, Add, e2) 
| BinOp (e1, Sub, e2) 
| BinOp (e1, Mul, e2)
| BinOp (e1, Div, e2) -> 
        let t1 = aux_analyse e1 env ctx in
        let t2 = aux_analyse e2 env ctx in
        unify t1 IntType;
        unify t2 IntType;
        IntType
| BinOp(e1, Comp, e2) -> begin
    let t1 = aux_analyse e1 env ctx in
    let t2 = aux_analyse e2 env ctx in
    let r_t = new_type_var ctx in
    unify t2 (FuncType (t1, r_t));
    r_t
end
| Tuple [] -> TupleType []
| Tuple (h::t) -> begin
    let h_type = aux_analyse h env ctx in
    let t_type = aux_analyse (Tuple t) env ctx in
    match t_type with
    | TupleType tl -> TupleType (h_type::tl)
    | _ -> failwith "unreachable"
end

and get_type v env = match StringMap.find_opt v env with
| Some t -> prune t
| None -> raise (Undefined_identifier v) 

and occurs_in_type t1 t2 =
    let t2 = prune t2 in
    match t2 with
    | Var t when t1 = t -> true
    | TupleType tl -> List.exists (fun x -> occurs_in_type t1 x) tl
    | FuncType (a, b) -> occurs_in_type t1 a || occurs_in_type t1 b
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
    | FuncType (a1, r1), FuncType (a2, r2) ->
            unify a1 a2;
            unify r1 r2
    | _ when a = b -> ()
    | _ -> raise (IncompatibleTypes (t1, t2))


let analyse e env ctx = aux_analyse e env ctx
