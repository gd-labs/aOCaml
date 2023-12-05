open Core

let () =
  let lines = Advent.read_lines "./inputs/day1.in" in
  List.fold lines ~init:0 ~f:(fun acc line ->
    let chars = String.to_array line in
    let numbers = Array.filter chars ~f:(Char.is_digit) in
    let number = 
      (Char.get_digit_exn numbers.(0)) * 10 + Char.get_digit_exn (Array.last numbers) in
    acc + number)
  |> Fmt.pr "Result: %d@."