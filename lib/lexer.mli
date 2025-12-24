type token = TT_Plus | TT_PPlus | TT_Minus | TT_Asterisk | TT_Slash | TT_Comma
| TT_LParen | TT_RParen | TT_LBrace | TT_RBrace | TT_Integer of int 
| TT_Float of float | TT_String of string | TT_Identifier of string | TT_Let 
| TT_Equals | TT_If | TT_Then | TT_Else

val lex_line : string -> token list
