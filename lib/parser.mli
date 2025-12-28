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

val parse : Lexer.token list -> expr
