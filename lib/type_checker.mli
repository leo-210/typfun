type types = IntegerType | StringType  | TupleType of (types list) * int 
| Any of int

val type_check : Parser.expr -> types
