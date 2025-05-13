(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/19 11:47:36 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:42:17 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let reverse_list list =
  let rec aux acc list =
    match list with [] -> acc | head :: tail -> aux (head :: acc) tail
  in
  aux [] list

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleobase : nucleotide =
  let base =
    match nucleobase with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _ -> None
  in

  match base with
  | None -> ("phosphate", "deoxyribose", None)
  | b -> ("phosphate", "deoxyribose", b)

type helix = nucleotide list

let generate_helix n : helix =
  if n <= 0 then []
  else
    let generate_nucleobase_char () =
      let nucleobase = Random.int 4 in
      match nucleobase with
      | 0 -> 'A'
      | 1 -> 'T'
      | 2 -> 'G'
      | 3 -> 'C'
      | _ -> 'X'
    in
    let rec aux acc i =
      if i = n then acc
      else aux (generate_nucleotide (generate_nucleobase_char ()) :: acc) (i + 1)
    in
    aux [] 0

let rec helix_to_string (h : helix) : string =
  let convert_nucleobase n =
    match n with A -> "A" | T -> "T" | C -> "C" | G -> "G" | None -> "X"
  in

  match h with
  | [] -> ""
  | head :: tail ->
      let a, b, base = head in
      convert_nucleobase base ^ helix_to_string tail

let rec complementary_helix (h : helix) : helix =
  let get_associated_base b =
    match b with A -> 'T' | T -> 'A' | C -> 'G' | G -> 'C' | None -> 'X'
  in

  let rec aux acc h =
    match h with
    | [] -> acc
    | head :: tail ->
        let _, _, base = head in
        aux (generate_nucleotide (get_associated_base base) :: acc) tail
  in
  reverse_list (aux [] h)

(* Function exclusively for testing purpose*)
let print_nucleotide (p, d, nb) =
  let nb_str =
    match nb with A -> "A" | T -> "T" | C -> "C" | G -> "G" | None -> "X"
  in
  Printf.printf "(%s, %s, %s)" p d nb_str

let () =
  print_newline ();
  print_string "===== Test to generate a helix with a length of -1 =====\n";
  print_string (helix_to_string (generate_helix (-1)));
  print_newline ();

  print_newline ();
  print_string "===== Test to generate a helix with a length of 1 =====\n";
  print_string (helix_to_string (generate_helix 1));
  print_newline ();

  print_newline ();
  print_string "===== Test to generate a helix with a length of 0 =====\n";
  print_string (helix_to_string (generate_helix 0));
  print_newline ();

  print_newline ();
  print_string "===== Test to generate a helix with a length of 10 =====\n";
  print_string (helix_to_string (generate_helix 10));
  print_newline ();

  print_newline ();
  print_string
    "===== Test to generate a complementary helix with an empty helix =====\n";
  print_string (helix_to_string (complementary_helix []));
  print_newline ();

  print_newline ();
  print_string
    "===== Test to generate a complementary helix with a helix of size 10 =====\n";
  let helix = generate_helix 10 in
  print_string (helix_to_string helix);
  print_newline ();
  print_string (helix_to_string (complementary_helix helix));
  print_newline ();

  print_newline ();
  print_string
    "===== Test to generate a complementary helix with a helix of size 50 =====\n";
  let helix = generate_helix 50 in
  print_string (helix_to_string helix);
  print_newline ();
  print_string (helix_to_string (complementary_helix helix));
  print_newline ()
