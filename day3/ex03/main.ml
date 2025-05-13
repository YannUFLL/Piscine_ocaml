(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/04 19:11:31 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:41:15 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  print_newline ();

  print_string "Testing Deck.newDeck:";
  print_newline ();
  let deck1 = Deck.newDeck () in
  let deck2 = Deck.newDeck () in
  print_string "Deck 1 (short view): ";
  List.iter (fun c -> print_string (c ^ " ")) (Deck.toStringList deck1);
  print_newline ();

  print_string "Deck 2 (short view): ";
  List.iter (fun c -> print_string (c ^ " ")) (Deck.toStringList deck2);
  print_newline ();

  print_newline ();

  print_string "Testing Deck.toStringList:";
  print_newline ();
  let string_deck = Deck.toStringList deck1 in
  List.iter (fun str -> print_string (str ^ " ")) string_deck;
  print_newline ();
  print_newline ();

  print_string "Testing Deck.toStringListVerbose:";
  print_newline ();
  let verbose_deck = Deck.toStringListVerbose deck1 in
  List.iter (fun str -> print_string (str ^ " ")) verbose_deck;
  print_newline ();
  print_newline ();

  print_string "Testing Deck.drawCard:";
  print_newline ();
  (match Deck.drawCard deck1 with
  | first_card, rest_deck ->
      print_string "First drawn card: ";
      print_endline (Deck.Card.toStringVerbose first_card);
      print_string "Remaining deck size: ";
      print_int (List.length (Deck.toStringList rest_deck));
      print_newline ()
  | exception Failure msg -> print_endline ("Error: " ^ msg));
  print_newline ();

  let rec empty_deck deck =
    match Deck.drawCard deck with
    | _, rest_deck -> empty_deck rest_deck
    | exception Failure _ -> deck
  in

  print_string "Emptying deck1...";
  print_newline ();
  let empty_deck1 = empty_deck deck1 in

  print_string "Testing Deck.drawCard on empty deck:";
  print_newline ();
  (match Deck.drawCard empty_deck1 with
  | first_card, rest_deck ->
      print_string "First drawn card: ";
      print_endline (Deck.Card.toStringVerbose first_card)
  | exception Failure msg -> print_endline ("Error: " ^ msg));
  print_newline ()
