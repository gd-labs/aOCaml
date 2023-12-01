let filename = "day1.in"

let lines input =
  let rec lines_acc input acc =
  try
    let line = input_line input in
    lines_acc input (line :: acc)
  with
    End_of_file -> acc
  in lines_acc input []

;;

let () =
  let channel = open_in filename in
  let lines = lines channel in
  lines |> List.iter (print_endline)
;;
