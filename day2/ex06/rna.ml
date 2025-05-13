(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/19 16:07:11 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:42:20 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let reverse_list list =
  let rec aux acc list =
    match list with [] -> acc | head :: tail -> aux (head :: acc) tail
  in
  aux [] list

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleobase : nucleotide =
  let base =
    match nucleobase with
    | 'A' -> Some A
    | 'T' -> Some T
    | 'C' -> Some C
    | 'G' -> Some G
    | 'U' -> Some U
    | _ -> None
  in

  match base with
  | None -> ("phosphate", "deoxyribose", None)
  | Some b -> ("phosphate", "deoxyribose", b)

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
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> "X"
  in

  match h with
  | [] -> ""
  | head :: tail ->
      let a, b, base = head in
      convert_nucleobase base ^ helix_to_string tail

let rec complementary_helix (h : helix) : helix =
  let get_associated_base b =
    match b with
    | A -> 'T'
    | T -> 'A'
    | C -> 'G'
    | G -> 'C'
    | U -> 'X'
    | None -> 'X'
  in

  let rec aux acc h =
    match h with
    | [] -> acc
    | head :: tail ->
        let _, _, base = head in
        aux (generate_nucleotide (get_associated_base base) :: acc) tail
  in
  reverse_list (aux [] h)

type rna = nucleobase list

let rec generate_rna (h : helix) : rna =
  let get_associated_base b =
    match b with A -> U | T -> A | C -> G | G -> C | U -> U | None -> None
  in

  let reverse_list list =
    let rec aux acc list =
      match list with [] -> acc | head :: tail -> aux (head :: acc) tail
    in
    aux [] list
  in

  let rec aux acc h =
    match h with
    | [] -> acc
    | head :: tail ->
        let _, _, base = head in
        aux (get_associated_base base :: acc) tail
  in
  reverse_list (aux [] h)

(* Function exclusively for testing purpose*)

let print_nucleotide (p, d, nb) =
  let nb_str =
    match nb with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> "X"
  in
  Printf.printf "(%s, %s, %s)" p d nb_str

let rec rna_to_string (h : rna) : string =
  let convert_nucleobase n =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> "X"
  in

  match h with
  | [] -> ""
  | base :: tail -> convert_nucleobase base ^ rna_to_string tail

let () =
  print_string "===== Test to generate rna with an empty tab [] =====\n";
  print_string (rna_to_string (generate_rna []));
  print_newline ();

  print_newline ();
  print_string "===== Test to generate an helix with an length of 7 =====\n";
  let helix1 = generate_helix 7 in
  print_string (helix_to_string helix1);
  print_newline ();

  print_newline ();
  print_string
    "===== Test to convert an helix with an length of 7 to rna =====\n";
  print_string (rna_to_string (generate_rna helix1));
  print_newline ();

  print_newline ();
  print_string "===== Test to generate an helix with an length of 50 =====\n";
  let helix2 = generate_helix 50 in
  print_string (helix_to_string helix2);
  print_newline ();

  print_newline ();
  print_string
    "===== Test to convert an helix with an length of 50 to rna =====\n";
  print_string (rna_to_string (generate_rna helix2));
  print_newline ();

  print_newline ();
  print_string "===== Test to convert an empty helix to rna =====\n";
  print_string (rna_to_string (generate_rna []));
  print_newline ()
