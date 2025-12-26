type type_variable
and types = IntType | StrType | BoolType | TupleType of (types list) 
| FuncType of (types list) * types | Var of type_variable

exception Undefined_identifier of string
exception Recursive_unification
exception IncompatibleTypes of types * types

module StringMap : (Map.S with type key = string)

val new_type_var : unit -> types
val type_to_string : types -> string

val analyse : Parser.expr -> types StringMap.t -> types
