let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

let sum : int list -> int =
  List.fold_left (+) 0

let prod : int list -> int =
  List.fold_left ( * ) 1

let is_digit (c: char): bool =
  '0' <= c && c <= '9'

let to_digit (c: char): int =
  Char.code c - Char.code '0'

(* A map from unique part names to lists of correspondings parts *)
module GearMap = Map.Make(Int)

type engine =
  { (* Input engine schema *)
    schema: string array;
    
    (* List of unique engine parts adjacent to this cell *)
    parts_labels: int list array array;
    
    (* A map from unique gears with exactly 2 adjacent numbers to the corresponding parts *)
    gear_map: int list GearMap.t;
  }

(* Directions from 10AM clockwise *)
let dirs =
  [| -1, -1 ; -1, 0 ; -1,  1 ;
      0, -1 ;          0,  1 ;
      1, -1 ;  1, 0 ;  1,  1 ;
  |]

let create_engine (lines: string list): engine =
  let schema = Array.of_seq (List.to_seq lines) in
  let rows_sz = Array.length schema in
  let cols_sz = String.length schema.(0) in
  let parts_labels = Array.make_matrix rows_sz cols_sz [] in
  let gear_map = GearMap.empty in
  { schema; parts_labels; gear_map }

let analyse_engine (engine: engine): engine =
  let rows_sz = Array.length engine.parts_labels in
  let cols_sz = Array.length engine.parts_labels.(0) in

  let in_bounds i j =
    0 <= i && i < rows_sz &&
    0 <= j && j < cols_sz
  in

  let unique_gear_id = ref 0 in
  let gear_map = ref engine.gear_map in

  for i = 0 to rows_sz - 1 do
    for j = 0 to cols_sz - 1 do
      if engine.schema.(i).[j] = '*' then (
        unique_gear_id := !unique_gear_id + 1;

        for d = 0 to Array.length dirs - 1 do
          let x, y = dirs.(d) in
          let new_i, new_j = i + x, j + y in
          if in_bounds new_i new_j && is_digit engine.schema.(new_i).[new_j] then (
            (* Printf.printf "Gear: %c, Gear id: %d, gear idx: %d, %d, part idx: %d, %d\n%!" engine.schema.(i).[j] !unique_part_id i j new_i new_j; *)
            engine.parts_labels.(new_i).(new_j) <- !unique_gear_id :: engine.parts_labels.(new_i).(new_j);
          )
        done;

        gear_map := GearMap.add !unique_gear_id [] !gear_map
      )
    done
  done;

  { schema = engine.schema;
    parts_labels = engine.parts_labels;
    gear_map = !gear_map;
  }

module IntSet = Set.Make(Int)

let get_parts (engine: engine): int list GearMap.t =
  let schema = engine.schema in

  let rows_sz = Array.length schema in
  let cols_sz = String.length schema.(0) in

  let add_part part part_gears gear_map =
    List.fold_left
      (fun gear_map part_gear -> 
        GearMap.update
          part_gear
          ( function
            | None -> None
            | Some parts -> Some (part :: parts)
          )
          gear_map
      )
      gear_map
      (IntSet.to_list part_gears)
  in

  let rec scan cur_part part_gears gear_map i j =
    if i >= rows_sz then
      gear_map
    else if j >= cols_sz then
      scan 0 IntSet.empty (add_part cur_part part_gears gear_map) (i + 1) 0
    else 
      match schema.(i).[j] with
      | c when is_digit c -> 
        scan 
          (cur_part * 10 + to_digit c) 
          (IntSet.add_seq (List.to_seq engine.parts_labels.(i).(j)) part_gears)
          gear_map
          i
          (j + 1)
      | _ -> 
        scan 0 IntSet.empty (add_part cur_part part_gears gear_map) i (j + 1) 
  in

  scan 0 IntSet.empty engine.gear_map 0 0

let calc_gear_ratios (parts_map: int list GearMap.t): int =
  parts_map
  |> GearMap.to_list
  |> List.map (fun (_, parts) -> if List.length parts = 2 then prod parts else 0)
  |> sum

let solve input = 
  lines input
  |> create_engine
  |> analyse_engine
  |> get_parts
  |> calc_gear_ratios

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"