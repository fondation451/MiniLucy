let ____read_file____ f =
  let fd = open_in f in
  let rec loop out =
    try
      loop (out ^ (input_line fd) ^ " ")
    with End_of_file ->
      close_in fd;
      String.split_on_char ' ' (String.sub out 0 ((String.length out) - 1))
  in loop ""
;;

let (&&&) a b =
  match a, b with
  |True, True -> True
  |_ -> False
;;

let (|||) a b =
  match a, b with
  |False, False -> False
  |_ -> True
;;

let my_not a =
  match a with
  |True -> False
  |False -> True
;;

let (<=>) a b = if a = b then True else False;;
let (<<>>) a b = if a <> b then True else False;;
let (<<>) a b = if a < b then True else False;;
let (<<=>) a b = if a <= b then True else False;;
let (<>>) a b = if a > b then True else False;;
let (<>=>) a b = if a >= b then True else False;;
