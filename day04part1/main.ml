let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

let sum : int list -> int =
  List.fold_left (+) 0


type card =
  {
    winning: int list;
    given: int list;
  }

(* " 41 48 83 86 17 " -> [41 48 83 86 17] *)
let parse_int_list input =
  input
  |> String.split_on_char ' '
  |> List.filter (fun s -> not (String.equal s ""))
  |> List.map int_of_string

(* [@@@warning "-8"] *)

(* 
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" 
  ->
  {winning: [41 48 83 86 17], given: [83 86  6 31 17  9 48 53]}
*)
let[@warning "-8"] parse_card (line: string): card =
  let [_card; numbers] = String.split_on_char ':' line in
  let [winning; given] = String.split_on_char '|' numbers in
  { 
    winning = parse_int_list winning ;
    given = parse_int_list given;
  }

let rec pow base power =
  if power <= 0 then 1 else base * pow base (power - 1)

let to_pow_2 n =
  if n <= 0 then 0 else pow 2 (n - 1)

let eval_card (card: card): int =
  card.given
  |> List.filter (fun given -> List.mem given card.winning)
  |> List.length
  |> to_pow_2

let solve input = 
  lines input
  |> List.map parse_card
  |> List.map eval_card
  |> sum

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"