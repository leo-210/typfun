type token = TT_Plus | TT_PPlus | TT_Minus | TT_Asterisk | TT_Slash | TT_LParen | TT_RParen 
| TT_LBracket | TT_RBracket | TT_LBrace | TT_RBrace 
| TT_Integer of int | TT_Float of float | TT_String of string 
| TT_Identifier of string

let keywords = 
    let ht = Hashtbl.create 16 in
    ht

let is_whitespace c =
    match c with
    | ' ' | '\t' | '\n' -> true
    | _ -> false

let is_digit c = 48 <= Char.code c && Char.code c <= 57

let is_letter c = (65 <= Char.code c && Char.code c <= 90) 
    || (97 <= Char.code c && Char.code c <= 122)
    || c = '_' (* it's a letter I don't know what you're taling about *)

let lex_plus s i tokens = 
    if !i + 1 < String.length s then begin
        if s.[!i+1] = '+' then begin
            i := !i + 1;
            tokens := TT_PPlus::!tokens
        end else tokens := TT_Plus::!tokens
    end else begin tokens := TT_Plus::!tokens end;
    i := !i + 1
    
(* TODO: lex float *)
let lex_number s i tokens =
    let n = ref 0 in
    while !i < String.length s && is_digit s.[!i] do
        n := 10 * !n + (Char.code s.[!i] - 48);
        i := !i + 1
    done;
    tokens := (TT_Integer !n)::!tokens

let lex_identifier s i tokens = 
    let t = ref "" in
    while !i < String.length s && (is_digit s.[!i] || is_letter s.[!i]) do
        t := !t ^ (String.make 1 s.[!i]);
        i := !i + 1
    done;
    match Hashtbl.find_opt keywords !t with
    | Some tt_k -> tokens := tt_k::!tokens
    | None -> tokens := (TT_Identifier !t)::!tokens
    
let lex_line (s :string) =
    let i = ref 0 in
    let tokens = ref [] in
    while !i < (String.length s) do
        let c = s.[!i] in
        match c with
        | '+' -> lex_plus s i tokens
        | '-' -> tokens := TT_Minus::!tokens; i := !i + 1
        | '*' -> tokens := TT_Asterisk::!tokens; i := !i + 1
        | '/' -> tokens := TT_Slash::!tokens; i := !i + 1
        | '(' -> tokens := TT_LParen::!tokens; i := !i + 1
        | ')' -> tokens := TT_RParen::!tokens; i := !i + 1
        | '[' -> tokens := TT_LBracket::!tokens; i := !i + 1
        | ']' -> tokens := TT_RBracket::!tokens; i := !i + 1
        | '{' -> tokens := TT_LBrace::!tokens; i := !i + 1
        | '}' -> tokens := TT_RBrace::!tokens; i := !i + 1
        | c when is_whitespace c -> i := !i + 1
        | c when is_digit c -> lex_number s i tokens
        | c when is_letter c -> lex_identifier s i tokens
        (* TODO: lex strings *)
        | _ -> failwith "Invalid token";
    done;
    List.rev !tokens

