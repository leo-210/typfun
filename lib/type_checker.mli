type types = IntegerType | StringType | ListType of types 
| TupleType of (types list) * int | Any of int

val type_check : Parser.expr -> types
