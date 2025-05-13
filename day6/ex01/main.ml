(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 15:30:05 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:37:35 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let dr = new Doctor.doctor "Giga Chad" (-10) (new People.people "Yann") in
  dr#talk;
  print_newline ();
  print_endline dr#to_string;
  dr#travel_in_time 2025 2050;
  print_endline dr#to_string;
  dr#travel_in_time 2050 1980;
  print_endline dr#to_string;
  dr#use_sonic_screwdriver
