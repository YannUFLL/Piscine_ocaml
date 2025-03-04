(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/04 15:11:40 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/04 18:51:57 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  print_newline ();

  print_string "Testing Card.toString with Card.allSpades:"; print_newline ();
  List.iter print_string (List.map Card.toString Card.allSpades);
  print_newline ();

  print_string "Testing Card.toString and Card.toStringVerbose with Card.allSpades:"; print_newline ();
  List.iter (fun e -> 
    print_string (Card.toString e); 
    print_string " ";
    print_string (Card.toStringVerbose e);
    print_newline ()) Card.allSpades;
  print_newline ();

  print_string "Testing Card.newCard with As and Club:"; print_newline ();
  print_string (Card.toStringVerbose (Card.newCard Card.Value.As Card.Color.Club));
  print_newline ();

  print_string "Testing Card.compare with J C and T10 S:"; print_newline ();
  print_int (Card.compare 
    (Card.newCard Card.Value.Jack Card.Color.Club) 
    (Card.newCard Card.Value.T10 Card.Color.Spade));
  print_newline ();

  print_string "Testing Card.compare with T 10 and J C:"; print_newline ();
  print_int (Card.compare 
    (Card.newCard Card.Value.T10 Card.Color.Spade)
    (Card.newCard Card.Value.Jack Card.Color.Club));
  print_newline ();

  print_string "Testing Card.compare with J C and J S:"; print_newline ();
  print_int (Card.compare 
    (Card.newCard Card.Value.Jack Card.Color.Club) 
    (Card.newCard Card.Value.Jack Card.Color.Spade));
  print_newline ();

  print_string "Testing Card.compare with J S and J C:"; print_newline ();
  print_int (Card.compare 
    (Card.newCard Card.Value.Jack Card.Color.Spade) 
    (Card.newCard Card.Value.Jack Card.Color.Club));
  print_newline ();

  print_string "Testing Card.compare with J C and J C:"; print_newline ();
  print_int (Card.compare 
    (Card.newCard Card.Value.Jack Card.Color.Club) 
    (Card.newCard Card.Value.Jack Card.Color.Club));
  print_newline ();
  print_newline ();

  print_string "Testing isOf function with QH:"; print_newline ();
  let card = Card.newCard Card.Value.Queen Card.Color.Heart in
  print_string ("isOf Queen Heart: " ^ string_of_bool (Card.isOf card Card.Color.Heart));
  print_newline ();
  print_string ("isOf Queen Spade: " ^ string_of_bool (Card.isOf card Card.Color.Spade));
  print_newline ();
  print_newline ();

  print_string "Testing isSpade, isHeart, isDiamond, isClub with KS:"; print_newline ();
  let card = Card.newCard Card.Value.King Card.Color.Spade in
  print_string ("isSpade: " ^ string_of_bool (Card.isSpade card)); print_newline ();
  print_string ("isHeart: " ^ string_of_bool (Card.isHeart card)); print_newline ();
  print_string ("isDiamond: " ^ string_of_bool (Card.isDiamond card)); print_newline ();
  print_string ("isClub: " ^ string_of_bool (Card.isClub card)); print_newline ();
  print_newline ();

  print_string "Testing max and min:"; print_newline ();
  let c1 = Card.newCard Card.Value.T10 Card.Color.Diamond in
  let c2 = Card.newCard Card.Value.King Card.Color.Club in
  print_string "Max(T10, K): "; print_string (Card.toString (Card.max c1 c2)); print_newline ();
  print_string "Min(T10, K): "; print_string (Card.toString (Card.min c1 c2)); print_newline ();
  print_newline ();

  print_string "Testing best:"; print_newline ();
  let cards = [
    Card.newCard Card.Value.T2 Card.Color.Spade;
    Card.newCard Card.Value.Jack Card.Color.Heart;
    Card.newCard Card.Value.King Card.Color.Diamond;
    Card.newCard Card.Value.As Card.Color.Club
  ] in
  print_string "Best card: "; print_string (Card.toStringVerbose (Card.best cards)); print_newline ();

  print_string "Testing best correct order:"; print_newline ();
  let cards = [
    Card.newCard Card.Value.T2 Card.Color.Spade;
    Card.newCard Card.Value.T2 Card.Color.Heart;
    Card.newCard Card.Value.T2 Card.Color.Diamond;
    Card.newCard Card.Value.T2 Card.Color.Club
  ] in
  print_string "Best card: "; print_string (Card.toStringVerbose (Card.best cards)); print_newline ();

  print_newline ();