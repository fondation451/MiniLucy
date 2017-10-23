(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
Clément PASCUTTO <clement.pascutto@ens.fr>
########
*)

{

  open Lexing;;
  open Eparser;;
  open East;;

  exception Lexical_error of string;;

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k) [
      "and", AND;
      "div", DIV;
      "else", ELSE;
      "fby", FBY;
      "if", IF;
      "let", LET;
      "merge", MERGE;
      "mod", MOD;
      "not", NOT;
      "or", OR;
      "then", THEN;
      "when", WHEN;

      "clock", CLK;
      "last", LAST;
      "until", UNTIL;
      "unless", UNLESS;
      "in", IN;
      "automaton", AUTOMATON;
      "continue", CONTINUE;
      "do", DO;
      "with", WITH;
      "reset", RESET;
      "match", MATCH;
      "every", EVERY;
    ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s
  ;;

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
  ;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
    | digit+ exponent
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  |'\n'
    {newline lexbuf; token lexbuf}
  |[' ' '\t' '\r']+
    {token lexbuf}
  |"--" [^ '\n']* ['\n']
    {newline lexbuf; token lexbuf}
  |"/*"
    {comment lexbuf; token lexbuf}
  |ident
    {id_or_keyword (lexeme lexbuf)}
  |digit+
    {CONST_INT (int_of_string (lexeme lexbuf))}
  |float
    {CONST_REAL (float_of_string (lexeme lexbuf))}
  |"-"
    {MINUS}
  |"+"
    {PLUS}
  |"*"
    {STAR}
  |"/"
    {SLASH}
  |">"
    {COMP Op_gt}
  |">="
    {COMP Op_ge}
  |"<"
    {COMP Op_lt}
  |"<="
    {COMP Op_le}
  |"<>"
    {NEQ}
  |"=>"
    {IMPL}
  |"->"
    {ARROW}
  |"("
    {LPAREN}
  |")"
    {RPAREN}
  |";"
    {SEMICOL}
  |"="
    {EQUAL}
  |","
    {COMMA}
  |"|"
    {VBAR}
  |_
    {raise (Lexical_error (lexeme lexbuf))}
  |eof
    {EOF}

and comment = parse
  |"*/" {()}
  |'\n' {newline lexbuf; comment lexbuf}
  |_ {comment lexbuf}
  |eof {raise (Lexical_error "unterminated comment")}
