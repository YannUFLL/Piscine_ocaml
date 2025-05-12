(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/12 15:43:39 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/12 16:05:15 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_chem_list lst =
  let rec aux = function
    | [] -> ()
    | [(mol, coef)] ->
        Printf.printf "%d %s" coef (mol#to_string)
    | (mol, coef)::t ->
        Printf.printf "%d %s + " coef (mol#to_string);
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

let test_combustion alks name =
  Printf.printf "=== %s ===\n" name;
  let rx = new Alkane_combustion.alkane_combustion alks in
  Printf.printf "Before balance: is_balanced = %b\n" rx#is_balanced;
  let balanced = rx#balance in
  Printf.printf "After  balance: is_balanced = %b\n" balanced#is_balanced;
  print_reaction balanced;
  Printf.printf "----------------------------------\n" 

let () =
  let methane = new Alkane.methane in
  let ethane  = new Alkane.ethane in

  test_combustion [methane] "Combustion du méthane" ;
  test_combustion [ethane] "Combustion de l'éthane" ;
  test_combustion [methane; ethane; ethane] "Mélange : CH4 + 2 C2H6" ;

  let already = new Alkane_combustion.alkane_combustion [methane]  in
  let fixed = already#balance in
  Printf.printf "=== Vérif. idempotence sur méthane ===\n";
  print_reaction fixed;