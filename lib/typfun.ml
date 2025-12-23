module Lexer = Lexer
module Parser = Parser
module Type_checker = Type_checker

let type_check_line s =
    let t = Lexer.lex_line s in
    let e = Parser.parse t in
    Type_checker.type_check e
