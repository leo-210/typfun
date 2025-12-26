type token = TT_Fn | TT_If | TT_Then | TT_Else | TT_Let | TT_In | TT_Or | TT_And
| TT_Int of int | TT_Str of string | TT_True | TT_False | TT_Ident of string
| TT_LParen | TT_RParen | TT_LBrace | TT_RBrace | TT_Comma | TT_Eq | TT_Eq_Eq
| TT_Bang | TT_Bang_Eq | TT_Lt | TT_Lt_Eq | TT_Gt | TT_Gt_Eq | TT_Plus 
| TT_Plus_Plus | TT_Minus | TT_Star | TT_Slash | TT_Dot | TT_Semicolon

let keywords = 
    let ht = Hashtbl.create 16 in
    Hashtbl.add ht "fn" TT_Fn;
    Hashtbl.add ht "if" TT_If;
    Hashtbl.add ht "then" TT_Then;
    Hashtbl.add ht "else" TT_Else;
    Hashtbl.add ht "let" TT_Let;
    Hashtbl.add ht "in" TT_In;
    Hashtbl.add ht "or" TT_Or;
    Hashtbl.add ht "and" TT_And;
    Hashtbl.add ht "true" TT_True;
    Hashtbl.add ht "false" TT_False;
    ht

let is_whitespace c =
    match c with
    | ' ' | '\t' | '\n' -> true
    | _ -> false

let is_digit c = 48 <= Char.code c && Char.code c <= 57

let is_letter c = (65 <= Char.code c && Char.code c <= 90) 
    || (97 <= Char.code c && Char.code c <= 122)

let lex_plus s i tokens = 
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '+' then begin
            i := !i + 1;
            tokens := TT_Plus_Plus::!tokens
        end else tokens := TT_Plus::!tokens
    end else begin tokens := TT_Plus::!tokens end;
    i := !i + 1

let lex_eq s i tokens =
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '=' then begin
            i := !i + 1;
            tokens := TT_Eq_Eq::!tokens
        end else tokens := TT_Eq::!tokens
    end else begin tokens := TT_Eq::!tokens end;
    i := !i + 1

let lex_lt s i tokens =
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '=' then begin
            i := !i + 1;
            tokens := TT_Lt_Eq::!tokens
        end else tokens := TT_Lt::!tokens
    end else begin tokens := TT_Lt::!tokens end;
    i := !i + 1

let lex_gt s i tokens =
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '=' then begin
            i := !i + 1;
            tokens := TT_Gt_Eq::!tokens
        end else tokens := TT_Gt::!tokens
    end else begin tokens := TT_Gt::!tokens end;
    i := !i + 1

let lex_bang s i tokens = 
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '=' then begin
            i := !i + 1;
            tokens := TT_Bang_Eq::!tokens
        end else tokens := TT_Bang::!tokens
    end else begin tokens := TT_Bang::!tokens end;
    i := !i + 1

(* TODO: lex float *)
let lex_number s i tokens =
    let n = ref 0 in
    while !i < String.length s && is_digit s.[!i] do
        n := 10 * !n + (Char.code s.[!i] - 48);
        i := !i + 1
    done;
    tokens := (TT_Int !n)::!tokens

let lex_identifier s i tokens = 
    let t = ref "" in
    while !i < String.length s && (
        is_digit s.[!i] || is_letter s.[!i] || s.[!i] = '_'
    ) do
        t := !t ^ (String.make 1 s.[!i]);
        i := !i + 1
    done;
    match Hashtbl.find_opt keywords !t with
    | Some tt_k -> tokens := tt_k::!tokens
    | None -> tokens := (TT_Ident !t)::!tokens
    
let lex_line (s :string) =
    let i = ref 0 in
    let tokens = ref [] in
    while !i < (String.length s) do
        let c = s.[!i] in
        match c with
        | '(' -> tokens := TT_LParen::!tokens; i := !i + 1
        | ')' -> tokens := TT_RParen::!tokens; i := !i + 1
        | '{' -> tokens := TT_LBrace::!tokens; i := !i + 1
        | '}' -> tokens := TT_RBrace::!tokens; i := !i + 1
        | ',' -> tokens := TT_Comma::!tokens; i := !i + 1
        | '=' -> lex_eq s i tokens
        | '!' -> lex_bang s i tokens
        | '<' -> lex_lt s i tokens
        | '>' -> lex_gt s i tokens
        | '+' -> lex_plus s i tokens
        | '-' -> tokens := TT_Minus::!tokens; i := !i + 1
        | '*' -> tokens := TT_Star::!tokens; i := !i + 1
        | '/' -> tokens := TT_Slash::!tokens; i := !i + 1
        | '.' -> tokens := TT_Dot::!tokens; i := !i + 1
        | ';' -> tokens := TT_Semicolon::!tokens; i := !i + 1
        | c when is_whitespace c -> i := !i + 1
        | c when is_digit c -> lex_number s i tokens
        | c when is_letter c -> lex_identifier s i tokens
        (* TODO: lex strings *)
        | _ -> failwith "Invalid token";
    done;
    List.rev !tokens

