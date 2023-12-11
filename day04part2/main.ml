let read_file (path: string): string =
  In_channel.with_open_bin path In_channel.input_all

let lines : string -> string list =
  String.split_on_char '\n'

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

let eval_card (card: card): int =
  card.given
  |> List.filter (fun given -> List.mem given card.winning)
  |> List.length

let calc_score (dp: int array) (cards: card list): int =
  let add_card_result (card_num: int) (card: card): unit =
    let card_score = eval_card card in

    for i = card_num + 1 to min (card_num + card_score) (Array.length dp - 1) do
      dp.(i) <- dp.(i) + dp.(card_num)
    done
  in

  (* Calculate the score of each card using dynamic programming *)
  List.iteri add_card_result cards;
  (* find the total number of won cards *)
  Array.fold_left (+) 0 dp

let solve input = 
  let lns = lines input in
  let dp = Array.make (List.length lns) 1 in
  let cards = List.map parse_card lns in
  calc_score dp cards

let () = 
  read_file "input.txt"
  |> solve
  |> Printf.printf "%d\n%!"