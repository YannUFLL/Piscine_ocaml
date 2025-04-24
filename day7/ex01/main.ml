(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 12:05:27 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 12:48:15 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
  print_endline "----- MOLECULE -----";
  new Molecule.water#name |> print_endline;
  new Molecule.water#formula |> print_endline;
  new Molecule.water#to_string |> print_endline;
  new Molecule.carbon_dioxide#to_string |> print_endline;
  new Molecule.ethanol#to_string |> print_endline;
  new Molecule.methane#to_string |> print_endline;
  new Molecule.glucose#to_string |> print_endline;
  print_endline "----- SORT TEST -----";
  new Molecule.trinitrotoluene#to_string |> print_endline;
  print_endline "----- EQUAL TEST -----";
  if new Molecule.water#compare (new Molecule.water) then print_endline "true" else print_endline "false";
  if new Molecule.water#compare (new Molecule.carbon_dioxide) then print_endline "true" else print_endline "false";
