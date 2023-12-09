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

let set_power (set: game_set): int =
  set.red * set.green * set.blue

let max_set (s1: game_set) (s2: game_set): game_set =
  {
    red   = max s1.red   s2.red; 
    green = max s1.green s2.green; 
    blue  = max s1.blue  s2.blue; 
  }

type game =
  { 
    sets: game_set list;
  }

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
  | [_game; sets] -> 
    let sets = parse_sets sets in
    Some { sets }
  | _ -> None

let to_min_set (sets: game_set list): game_set =
  match sets with
  | [] -> init_game_set
  | set :: sets -> List.fold_left max_set set sets

let to_min_power_set (game: game): int =
    to_min_set game.sets |> set_power

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
  |> List.map to_min_power_set
  |> sum

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"