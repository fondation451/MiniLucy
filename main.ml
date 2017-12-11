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
let clock_only = ref false;;
let normalize_only = ref false;;
let schedule_only = ref false;;
let object_only = ref false;;

let verbose = ref false;;

let input_file = ref "";;
let output_file = ref "";;

let usage = "usage: scala [options] file.lus";;

let set_file f s = f := s;;

let options = [
  "--parse-only", Arg.Set parse_only, "  Execute only syntactic analysis";
  "--type-only", Arg.Set type_only, "  Execute only typing";
  "--clock-only", Arg.Set clock_only, "  Execute only clock verification";
  "--normalize-only", Arg.Set normalize_only, "  Execute only normalization";
  "--schedule-only", Arg.Set schedule_only, "  Execute only schedule";
  "--object-only", Arg.Set object_only, "  Execute only transformation to object code";
  "-v", Arg.Set verbose, "  Verbose mode"
];;

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !input_file l (c-1) c
;;

let print_separation () =
  print_newline ();
  print_newline ();
  print_string "####################################";
  print_newline ();
  print_string "####################################";
  print_newline ();
  print_newline ()
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

  print_separation ();

  let f_name = Filename.remove_extension !input_file in

  let f = open_in !input_file in
  let buf = Lexing.from_channel f in

  let p_lustre =
    if (Filename.check_suffix !input_file ".lus") then begin
      try
        (* PARSING LUS *)
        Parser.file Lexer.token buf
      with
      |Lexer.Lexical_error(str) ->
        (localisation (Lexing.lexeme_start_p buf);
        eprintf "Lexing error@.";
        Printf.printf "%s\n" str;
        exit 1)
    end else begin
      (* PARSING ELUS *)

      let p_elustre = Eparser.file Elexer.token buf in
      close_in f;

      if !verbose then begin
        print_string "    (SYNTATIC ANALYSIS ELUS)\n";
        Elustre_printer.print_elustre p_elustre;
        print_separation ()
      end;

      if !parse_only then exit 0;

      (* TRANSLATION TO AST *)

      let ast_lustre = Easttoast.translate_file p_elustre in

      if !verbose then begin
        print_string "    (TRANSLATION TO LUS)\n";
        Lustre_printer.print_lustre ast_lustre;
        print_separation ()
      end;

      ast_lustre
    end in

    close_in f;

    if !verbose then begin
      print_string "    (SYNTATIC ANALYSIS)\n";
      Lustre_printer.print_lustre p_lustre;
      print_separation ()
    end;

    if !parse_only then begin
      exit 0
    end;

    try
      (* TYPING *)
      let p = Type.type_file p_lustre in

      if !verbose then begin
        print_string "    (TYPING)\n";
        Lustre_printer.print p;
        print_separation ()
      end;

      if !type_only then begin
        exit 0
      end;

      (* CLOCKING *)
      let pc = Clocking.check_clock_file p in

      if !verbose then begin
        print_string "    (CLOCKING)\n";
        Lustre_printer.print pc;
        print_separation ()
      end;

      if !clock_only then begin
        exit 0;
      end;

      (* NORMALIZATION *)
      let pn = Normalize.normalize_file pc in

      if !verbose then begin
        print_string "    (NORMALIZATION)\n";
        Lustre_printer.print pn;
        print_separation ()
      end;

      if !normalize_only then begin
        exit 0;
      end;

      (* SCHEDULING *)
      let ps = Schedule.schedule_file pn in

      if !verbose then begin
        print_string "    (SCHEDULING)\n";
        Lustre_printer.print ps;
        print_separation ()
      end;

      if !schedule_only then begin
        exit 0;
      end;

      (* OBJECT *)
      let po = Object.trans_file ps in

      if !verbose then begin
        print_string "    (OBJECT)\n";
        Lustre_printer.print_obj_file po;
        print_separation ()
      end;

      if !object_only then begin
        exit 0;
      end;

      let source = To_c.file_to_c po in

      Export_source.export (f_name ^ ".c") source;

      exit 0;
    with
      |Parser.Error ->
        localisation (Lexing.lexeme_start_p buf);
    	  eprintf "Syntax error@.";
        exit 1
      |Type.Type_Error(str) ->
        eprintf "Type error@.";
        Printf.printf "%s\n" str;
        exit 1
      |Clocking.Bad_Clock(loc, str) ->
        localisation (fst loc);
        eprintf "Clock error@.";
        Printf.printf "%s\n" str;
        exit 1
      |To_c.No_Main ->
        eprintf "C Generation error@.";
        Printf.printf "No Main\n";
        exit 1;
;;
