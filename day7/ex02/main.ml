(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 12:05:27 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 13:20:12 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
  print_endline "----- ALKANE -----";
  new Alkane.methane#to_string |> print_endline;
  new Alkane.ethane#to_string |> print_endline;
  new Alkane.octane#to_string |> print_endline;
  print_endline "----- COMPARE -----";
  if new Alkane.methane#compare new Alkane.methane then "true" |> print_endline else "false" |> print_endline;
  if new Alkane.methane#compare new Alkane.ethane then "true" |> print_endline else "false" |> print_endline

