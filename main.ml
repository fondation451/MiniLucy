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

open Printf;;
open Lexing;;
open Format;;
(*open Lustre_printer*)

let parse_only = ref false;;
let type_only = ref false;;

let verbose = ref false;;

let input_file = ref "";;
let output_file = ref "";;

let usage = "usage: scala [options] file.lus";;

let set_file f s = f := s;;

let options = [
  "--parse-only", Arg.Set parse_only, "  Execute only syntactic analysis";
  "-v", Arg.Set verbose, "  Verbose mode"
];;

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !input_file l (c-1) c
;;

let () =
  Arg.parse options (set_file input_file) usage;

  if !input_file = "" then begin
    eprintf "No file to compile\n@?";
    Arg.usage options usage;
    exit 1
  end;

  if not (Filename.check_suffix !input_file ".lus"
      || (Filename.check_suffix !input_file ".elus")) then begin
    eprintf "The input file must be a .lus or a .elus file\n@?";
    Arg.usage options usage;
    exit 1
  end;


  let f = open_in !input_file in
  let buf = Lexing.from_channel f in

  if (Filename.check_suffix !input_file ".lus") then begin
    try
      let p_lustre = Parser.file Lexer.token buf in
      let p = Ast_lustre_to_ast.convert p_lustre in
      close_in f;

      if !parse_only then begin
        Lustre_printer.print_lustre p_lustre;
        print_newline ();
        print_newline ();
        print_newline ();
        Lustre_printer.print p;
        exit 0;
      end;

      exit 0;
    with
      |Lexer.Lexical_error(str) ->
        (localisation (Lexing.lexeme_start_p buf);
        eprintf "Lexing error@.";
        Printf.printf "%s\n" str;
        exit 1)
      |Parser.Error ->
        localisation (Lexing.lexeme_start_p buf);
    	  eprintf "Syntax error@.";
        exit 1
      (*|_ ->
        eprintf "Unknown Error@.";
        exit 2*)
  end else begin
    try
      let p_elustre = Eparser.file Elexer.token buf in
      close_in f;

      if !parse_only then begin
        Elustre_printer.print_elustre p_elustre;
      end;

      exit 0;
    with
      |Elexer.Lexical_error(str) ->
        (localisation (Lexing.lexeme_start_p buf);
        eprintf "Lexing error@.";
        Printf.printf "%s\n" str;
        exit 1)
      |Eparser.Error ->
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Syntax error@.";
        exit 1
      (*|_ ->
        eprintf "Unknown Error@.";
        exit 2*)
  end
;;
