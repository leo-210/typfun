type bin_op = 
    | Addition
    | Substraction
    | Multiplication
    | Division
    | Concatenation

type expr = 
    | Tuple of expr list
    | List of expr list
    | BinOp of expr * bin_op * expr
    | UnaryNeg of expr
    | IntegerLiteral of int 
    | StringLiteral of string 
    | Identifier of string

exception MissingClosingParen
exception UnexpectedToken

val parse : Lexer.token list -> expr
