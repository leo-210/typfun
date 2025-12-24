open Parser

type types = IntegerType | StringType | TupleType of (types list) * int 
| Any of int

type inference_context = {
    mutable next_any: int;
    valuations: (string, types) Hashtbl.t;
    any_corr: (int, types) Hashtbl.t
}

let init_ctx () = {
    next_any = 0;
    valuations = Hashtbl.create 16;
    any_corr = Hashtbl.create 16
}

let rec true_t t ctx = match t with 
| Any x -> begin
    match Hashtbl.find_opt ctx.any_corr x with
    | Some t' -> true_t t' ctx
    | None -> t
end
| _ -> t

let rec infer t1 t2 ctx = match t1, t2 with
| t1, t2 when t1 = t2 -> true
| Any x, t | t, Any x ->  
        let t' = true_t (Any x) ctx in
        if t' = Any x then begin
            Hashtbl.add ctx.any_corr x t;
            true
        end else infer t t' ctx
| TupleType ([], _), TupleType ([], _) -> true
| TupleType (h1::t1, x), TupleType (h2::t2, y) when x = y ->
        (infer h1 h2 ctx) && (infer (TupleType (t1, x - 1)) (TupleType (t2, y - 2)) ctx) 
| _ -> false

let rec check (e : expr) ctx = match e with
| IntegerLiteral _ -> IntegerType
| StringLiteral _ -> StringType
| Identifier s -> begin 
    match Hashtbl.find_opt ctx.valuations s with
    | None ->
        Hashtbl.add ctx.valuations s (Any ctx.next_any);
        ctx.next_any <- ctx.next_any + 1;
        Any (ctx.next_any - 1)
    | Some t -> t
end
| BinOp (e1, Addition, e2) 
| BinOp (e1, Substraction, e2) 
| BinOp (e1, Multiplication, e2)
| BinOp (e1, Division, e2) -> begin
    match_t e1 IntegerType ctx;
    match_t e2 IntegerType ctx;
    IntegerType
end
| BinOp (e1, Concatenation, e2) -> begin
    match_t e1 StringType ctx;
    match_t e2 StringType ctx;
    StringType
end
| UnaryNeg e' -> begin
    match_t e' IntegerType ctx;
    IntegerType
end 
| Tuple [] -> TupleType ([], 0)
| Tuple (h::t) -> begin
    let head_t = check h ctx in
    let tail_t = check (Tuple t) ctx in
    match tail_t with
    | TupleType (l, n) -> TupleType (head_t::l, n + 1)
    | _ -> failwith "unreachable"
end
| _ -> failwith "not implemented" 

and match_t e t ctx =
    if not (infer (check e ctx) t ctx) then
        failwith "incompatible types"

let rec unwrap t ctx = 
    let t = true_t t ctx in
    match t with
    | TupleType ([], _) -> t
    | TupleType (h::t, n) -> begin
        match unwrap (TupleType (t, n - 1)) ctx with
        | TupleType (l, _) -> TupleType ((unwrap h ctx)::l, n)
        | _ -> failwith "unreachable"
    end
    | _ -> t


let type_check e = 
    let ctx = init_ctx () in
    let t = check e ctx in
    unwrap t ctx

