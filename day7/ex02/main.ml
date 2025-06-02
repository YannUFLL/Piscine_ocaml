(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 12:05:27 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/02 12:14:58 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  print_endline "----- NAME -----";
(new Alkane.methane)#name |> print_endline;
(new Alkane.ethane)#name |> print_endline;
  (new Alkane.octane)#name |> print_endline;
  print_endline "----- FORMULA -----";
(new Alkane.methane)#formula |> print_endline;
(new Alkane.ethane)#formula |> print_endline;
  (new Alkane.octane)#formula |> print_endline;
  print_endline "----- TO_STRING -----";
  (new Alkane.methane)#to_string |> print_endline;
  (new Alkane.ethane)#to_string |> print_endline;
  (new Alkane.octane)#to_string |> print_endline;
  print_endline "----- EQUALS -----";
  if (new Alkane.methane)#equals (new Alkane.methane :> Molecule.molecule) then
    "true" |> print_endline
  else "false" |> print_endline;
  if (new Alkane.methane)#equals (new Alkane.ethane :> Molecule.molecule) then
    "true" |> print_endline
  else "false" |> print_endline;
