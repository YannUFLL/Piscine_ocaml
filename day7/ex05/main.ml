(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/12 15:43:39 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:25:12 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_chem_list lst =
  let rec aux = function
    | [] -> ()
    | [(mol, coef)] ->
        Printf.printf "%d %s" coef (mol#formula)
    | (mol, coef)::t ->
        Printf.printf "%d %s + " coef (mol#formula);
        aux t
  in
  aux lst;
  Printf.printf "\n"

let print_reaction reaction =
  Printf.printf "Reactants: ";
  print_chem_list (reaction#get_start);
  Printf.printf "Products:  ";
  print_chem_list (reaction#get_result);
  Printf.printf "\n"

let test_incomplete_result alks name =
  Printf.printf "=== %s ===\n" name;
  let rx = new Alkane_combustion.alkane_combustion alks in
  let balanced = rx#balance in
  print_string "Normal combustion reactants: "; print_chem_list balanced#get_start; 
  print_string "Normal combustion products : "; print_chem_list balanced#get_result; 
  let i_result = rx#get_incomplete_results in 
  let rec print_i_result r = match r with
  | (o, l) :: tail -> o |> Printf.printf "Number of oxygen : %d -> " ;  print_chem_list (l); print_i_result (tail)
  |  [] -> () in 
  print_i_result i_result; 
  print_newline ()
  

let () =
  let methane = new Alkane.methane in
  let ethane  = new Alkane.ethane in

  test_incomplete_result [methane] "Incomplete combustion du méthane" ;
  test_incomplete_result [ethane] "Incomplete combustion de l'éthane" ;
  test_incomplete_result [methane; ethane; ethane] " Incomplete combustion mélange : CH4 + 2 C2H6" ;