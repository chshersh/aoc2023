let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

let is_digit (c: char): bool =
  '0' <= c && c <= '9'

let to_digit (c: char): int =
  Char.code c - Char.code '0'

let decode (line: string): int =
  let rec find_first (i: int): int =
    if is_digit line.[i]
      then to_digit line.[i]
      else find_first (i + 1)
  in
  let rec find_last (i: int): int =
    if is_digit line.[i]
      then to_digit line.[i]
      else find_last (i - 1)
  in
  let first_digit = find_first 0 in
  let last_digit = find_last (String.length line - 1) in
  first_digit * 10 + last_digit

let sum : int list -> int =
  List.fold_left (+) 0

(* 
1. Split into lines
2. Find the first and the last digit and combine them into a number
3. Add all numbers
*)
let solve input = 
  lines input
  |> List.map decode
  |> sum

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"