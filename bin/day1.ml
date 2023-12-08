open Core
open List

let part_one lines =
  fold lines ~init:0 ~f:(fun acc line ->
    let nums =
      line
      |> String.to_list
      |> filter_map ~f:(fun ch ->
        if Char.is_digit ch then Char.get_digit ch else None)
    in
    acc + (hd_exn nums * 10) + last_exn nums)
;;

let cases =
  [ "one", 1
  ; "two", 2
  ; "three", 3
  ; "four", 4
  ; "five", 5
  ; "six", 6
  ; "seven", 7
  ; "eight", 8
  ; "nine", 9
  ]
;;

let part_two lines =
  fold lines ~init:0 ~f:(fun acc line ->
    let nums =
      line
      |> String.foldi ~init:[] ~f:(fun pos acc ch ->
        let num =
          if Char.is_digit ch then Char.get_digit
          else
            find_map cases ~f:(fun (substr, value) ->
              match String.substr_index ~pos line ~pattern:substr with
              | Some matched when matched = pos -> Some value
              | _ -> None)
        in
        acc @ [ num ])
      |> filter_opt
    in
    acc + (hd_exn nums * 10) + last_exn nums)
;;

let () =
  let lines = Advent.read_lines "./inputs/day1.in" in
  Fmt.pr "Part 1: %d@." (part_one lines);
  Fmt.pr "Part 2: %d@." (part_two lines)
;;
