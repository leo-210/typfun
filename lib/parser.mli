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

val parse : Lexer.token list -> expr
