(* vim: set ts=2 sw=2: *)
{
  open Xi_lib
  open Parser
  open Parser_utils
  open Lexing

  type token = Parser.token

  let handleError pos token =
      let exc = InvalidToken (mkLocation pos, token) in
      raise exc

  }

  let whitespace = [' ' '\t']
  let digit = ['0'-'9']
  let alnum = ['_' 'A'-'Z' 'a'-'z' '0'-'9']
  let rstring = alnum*
  let identifier = ['a'-'z' '_' 'A' - 'Z'] alnum*

  rule token = parse
      | ['\n']
      { new_line lexbuf; token lexbuf }

      | whitespace+
      { token lexbuf }

      | "//"
      { line_comment lexbuf }

      | eof
      { EOF }

      | '+'                     { PLUS }
      | '-'                     { MINUS }
      | '*'                     { MULT }
      | '/'                     { DIV }
      | '%'                     { MOD }
      | '&'                     { BIN_AND }
      | '|'                     { BIN_OR }
      | '!'                     { NOT }
      | '('                     { LPAREN }
      | ')'                     { RPAREN }
      | '{'                     { LBRACE }
      | '}'                     { RBRACE }
      | '['                     { LBRACKET }
      | ']'                     { RBRACKET }
      | ','                     { COMMA }
      | ':'                     { COLON }
      | ';'                     { SEMICOLON }
      | "=="                    { EQ }
      | "!="                    { NEQ }
      | "<="                    { LTE }
      | '<'                     { LT }
      | ">="                    { GTE }
      | '>'                     { GT }
      | '='                     { ASSIGN }
      | '_'                     { UNDERSCORE }
      | "true"                  { BOOL (true) }
      | "false"                 { BOOL (false) }
      | "length"                { LENGTH }
      | "int"                   { INT_T }
      | "bool"                  { BOOL_T }
      | "if"                    { IF }
      | "else"                  { ELSE }
      | "while"                 { WHILE }
      | "return"                { RET }
      | "'"                     { esc_char lexbuf }
      | "\""                    { esc_string "" lexbuf }

      | identifier as id
      { IDENTIFIER (id) }

      | digit+
      { INT (Int32.of_int (int_of_string (Lexing.lexeme lexbuf))) }

      | _
      { handleError (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme lexbuf) }

  and line_comment = parse
      | '\n'
      { new_line lexbuf; token lexbuf }

      | eof
      { EOF }

      | _
      { line_comment lexbuf }

  and esc_string buf = parse
      | '\n'                { handleError (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme lexbuf) }
      | "\\n"               { esc_string (buf ^ "\n") lexbuf }
      | "\\t"               { esc_string (buf ^ "\t") lexbuf }
      | "\\'"               { esc_string (buf ^ "\'") lexbuf }
      | "\\\""              { esc_string (buf ^ "\"") lexbuf }
      | "\\\\"              { esc_string (buf ^ "\\") lexbuf }
      | "\""                { STRING (buf) }
      | _ as c              { esc_string (buf ^ (String.make 1 c)) lexbuf }
  and esc_char = parse
      | "\\n"               { ret_char '\n' lexbuf }
      | "\\\'"              { ret_char '\'' lexbuf }
      | "\\\\"              { ret_char '\\' lexbuf }
      | ['\'' '\\' '\n']    { handleError (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme lexbuf) }
      | _ as c              { ret_char c lexbuf }
  and ret_char buf = parse
      | '\''                { CHAR buf }
      | _ as c              { handleError (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme lexbuf) }

