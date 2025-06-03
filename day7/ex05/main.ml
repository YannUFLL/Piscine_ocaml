(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/12 15:43:39 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/03 11:48:43 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let subscript_digit_str c =
  match c with
  | '0' -> "₀"
  | '1' -> "₁"
  | '2' -> "₂"
  | '3' -> "₃"
  | '4' -> "₄"
  | '5' -> "₅"
  | '6' -> "₆"
  | '7' -> "₇"
  | '8' -> "₈"
  | '9' -> "₉"
  | _ -> String.make 1 c

let prettify_formula formula =
  let buffer = Buffer.create (String.length formula) in
  String.iter (fun c -> Buffer.add_string buffer (subscript_digit_str c)) formula;
  Buffer.contents buffer
let print_chem_list lst =
  let rec aux = function
    | [] -> ()
    | [ (mol, coef) ] ->
        Printf.printf "%d %s" coef (prettify_formula mol#formula)
    | (mol, coef) :: t ->
        Printf.printf "%d %s + " coef (prettify_formula mol#formula);
        aux t
  in
  aux lst;
  Printf.printf "\n"


let print_reaction reaction =
  Printf.printf "Reactants: ";
  print_chem_list reaction#get_start;
  Printf.printf "Products:  ";
  print_chem_list reaction#get_result;
  Printf.printf "\n"

let test_incomplete_result alks name =
  Printf.printf "=== %s ===\n" name;
  let rx = new Alkane_combustion.alkane_combustion alks in
  let balanced = rx#balance in
  print_string "Complete combustion reactants: ";
  print_chem_list balanced#get_start;
  print_string "Complete combustion products : ";
  print_chem_list balanced#get_result;
  print_string "== List of incomplete combustions ==\n";
  let i_result = rx#get_incomplete_results in
  let rec print_i_result r =
    match r with
    | (o, l) :: tail ->
        o |> Printf.printf "Oxygen atoms: %d → ";
        print_chem_list l;
        print_i_result tail
    | [] -> ()
  in
  print_i_result i_result;
  print_newline ()

let () =
  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in
  let propane = new Alkane.propane in

  test_incomplete_result [ methane ] "Incomplete combustion of methane";
  test_incomplete_result [ ethane ] "Incomplete combustion of ethane";
  test_incomplete_result [ propane ] "Incomplete combustion of propane";
  test_incomplete_result
    [ methane; ethane; ethane ]
    "Incomplete combustion of the mixture: CH₄ + 2 C₂H₆"
