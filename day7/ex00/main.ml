(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/23 16:59:35 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 12:36:43 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
new Atom.hydrogen#to_string |> print_endline;
new Atom.helium#to_string |> print_endline;
new Atom.lithium#to_string |> print_endline;
new Atom.carbon#to_string |> print_endline;
new Atom.iron#to_string |> print_endline;
new Atom.oxygen#to_string |> print_endline;
new Atom.oxygen#to_string |> print_endline;
new Atom.oxygen#to_string |> print_endline;
if (new Atom.hydrogen#equal (new Atom.hydrogen)) then print_endline  "true" else print_endline "false";
if (new Atom.hydrogen#equal (new Atom.helium)) then print_endline  "true" else print_endline "false";