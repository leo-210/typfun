type type_variable
and types = IntType | StrType | BoolType | TupleType of (types list) 
| Var of type_variable

val type_to_string : types -> string

val analyse : Parser.expr -> types
