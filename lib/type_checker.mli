type type_variable
and types = IntType | StrType | BoolType | TupleType of (types list) 
| FuncType of types * types | Var of type_variable

type context

exception Undefined_identifier of string
exception Recursive_unification
exception IncompatibleTypes of types * types

module StringMap : (Map.S with type key = string)

val init_ctx : unit -> context

val new_type_var : context -> types
val type_to_string : types -> context -> string

val analyse : Parser.expr -> types StringMap.t -> context -> types
