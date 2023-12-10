let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

let sum : int list -> int =
  List.fold_left (+) 0

let is_digit (c: char): bool =
  '0' <= c && c <= '9'

let to_digit (c: char): int =
  Char.code c - Char.code '0'

let is_symbol (c: char): bool =
  c != '.' && not (is_digit c)

type engine =
  { schema: string array;
    parts: bool array array;
  }

(* Directions from 10AM clockwise *)
let dirs =
  [ -1, -1 ; -1, 0 ; -1,  1 ;
     0, -1 ;          0,  1 ;
     1, -1 ;  1, 0 ;  1,  1 ;
  ]

let map_parts (engine: engine): engine =
  let parts = engine.parts in

  let rows_sz = Array.length parts in
  let cols_sz = Array.length parts.(0) in

  let in_bounds i j =
    0 <= i && i < rows_sz &&
    0 <= j && j < cols_sz
  in

  for i = 0 to rows_sz - 1 do
    for j = 0 to cols_sz - 1 do
      if is_symbol engine.schema.(i).[j] then
        List.iter 
          (fun (x, y) ->
            let new_i, new_j = i + x, j + y in
            if in_bounds new_i new_j then
              parts.(new_i).(new_j) <- true
          )
          dirs
    done
  done;

  engine

let analyse_engine (lines: string list): engine =
  let schema = Array.of_seq (List.to_seq lines) in
  let rows_sz = Array.length schema in
  let cols_sz = String.length schema.(0) in
  let parts = Array.make_matrix rows_sz cols_sz false in
  { schema; parts }

let get_parts (engine: engine): int list =
  let schema = engine.schema in

  let rows_sz = Array.length schema in
  let cols_sz = String.length schema.(0) in

  let add_num num is_part parts =
    if is_part && num > 0 then 
      num :: parts
    else
      parts
  in

  let rec scan cur_num is_part parts i j =
    if i >= rows_sz then
      parts
    else if j >= cols_sz then
      scan 0 false (add_num cur_num is_part parts) (i + 1) 0
    else 
      match schema.(i).[j] with
      | c when is_digit c -> 
        scan 
          (cur_num * 10 + to_digit c) 
          (is_part || engine.parts.(i).(j))
          parts
          i
          (j + 1)
      | _ -> 
        scan 0 false (add_num cur_num is_part parts) i (j + 1) 
  in

  scan 0 false [] 0 0

let solve input = 
  lines input
  |> analyse_engine
  |> map_parts
  |> get_parts
  |> sum

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"