type token = TT_Fn | TT_If | TT_Then | TT_Else | TT_Let | TT_In | TT_Or | TT_And
| TT_Int of int | TT_Str of string | TT_True | TT_False | TT_Ident of string
| TT_LParen | TT_RParen | TT_LBrace | TT_RBrace | TT_Comma | TT_Eq | TT_Eq_Eq
| TT_Bang | TT_Bang_Eq | TT_Lt | TT_Lt_Eq | TT_Gt | TT_Gt_Eq | TT_Plus 
| TT_Plus_Plus | TT_Minus | TT_Star | TT_Slash | TT_Dot


val lex_line : string -> token list
