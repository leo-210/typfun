type token = TT_Plus | TT_PPlus | TT_Minus | TT_Asterisk | TT_Slash | TT_LParen | TT_RParen 
| TT_LBracket | TT_RBracket | TT_LBrace | TT_RBrace 
| TT_Integer of int | TT_Float of float | TT_String of string 
| TT_Identifier of string

val lex_line : string -> token list
