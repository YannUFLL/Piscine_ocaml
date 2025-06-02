(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/23 16:59:35 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/02 14:32:20 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  print_endline "----- ATOM -----";
  (new Atom.hydrogen)#name |> print_endline;
  (new Atom.hydrogen)#symbol |> print_endline;
  (new Atom.hydrogen)#to_string |> print_endline;
  (new Atom.helium)#to_string |> print_endline;
  (new Atom.lithium)#to_string |> print_endline;
  (new Atom.carbon)#to_string |> print_endline;
  (new Atom.iron)#to_string |> print_endline;
  (new Atom.oxygen)#to_string |> print_endline;
  print_endline "----- EQUAL TEST -----";
  if (new Atom.hydrogen)#equal (new Atom.hydrogen) then print_endline "true"
  else print_endline "false";
  if (new Atom.hydrogen)#equal (new Atom.helium) then print_endline "true"
  else print_endline "false"
