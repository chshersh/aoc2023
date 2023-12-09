let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

type game_set =
  { red: int;
    green: int;
    blue: int;
  }

let init_game_set : game_set =
  { red = 0;
    green = 0;
    blue = 0;
  }

type game =
  { id: int;
    sets: game_set list;
  }

let is_valid_set (set: game_set): bool =
  set.red <= 12 && set.green <= 13 && set.blue <= 14

let is_valid_game ({sets; _}: game): bool =
  List.for_all is_valid_set sets

(* "Game 11" -> 11 *)
let parse_game_id (s: string): int =
  match String.split_on_char ' ' s with
  | [_; n] -> int_of_string n
  | _ -> -1

(* "3 blue, 4 red" -> { blue: 3, red: 4, green: 0} *)
let parse_set (s: string): game_set =
  let parse_cube (acc: game_set) (s: string): game_set =
    let input = String.trim s in
    match String.split_on_char ' ' input with
    | [n; "red"]   -> { acc with red   = int_of_string n }
    | [n; "green"] -> { acc with green = int_of_string n }
    | [n; "blue"]  -> { acc with blue  = int_of_string n }
    | _ -> acc
  in
  String.trim s
  |> String.split_on_char ','
  |> List.fold_left parse_cube init_game_set

(* 
   "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   ->
  [ { blue: 3, red: 4, green: 0}; ...]
*)
let parse_sets (s: string): game_set list =
  String.split_on_char ';' s
  |> List.map parse_set 

let parse (line: string): game option =
  match String.split_on_char ':' line with
  | [game; sets] -> 
    let id = parse_game_id game in
    let sets = parse_sets sets in
    Some { id; sets }
  | _ -> None

let sum : int list -> int =
  List.fold_left (+) 0

(* 
1. Split into lines
2. Parse every line into game
3. Filter all valid
4. Add all game ids
*)
let solve input = 
  lines input
  |> List.filter_map parse
  |> List.filter is_valid_game
  |> List.map (fun game -> game.id)
  |> sum

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"