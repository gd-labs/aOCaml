let parse input =
  let rec parse_acc input acc =
    match input_line input with
    | exception End_of_file -> acc
    | line -> parse_acc input (line :: acc)
  in
  parse_acc input []
;;

let () =
  let channel = open_in "bin/day1.in" in
  let lines = parse channel in
  close_in channel;
  lines |> List.iter (print_endline)
;;