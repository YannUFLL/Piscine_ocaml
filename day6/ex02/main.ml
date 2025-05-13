(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 18:01:24 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:37:42 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let dr = new Doctor.doctor "Octopus" 80 (new People.people "Victoume") in
  let dalek = new Dalek.dalek in
  let bob = new People.people "Bob" in
  dalek#exterminate bob;
  dalek#exterminate dr#get_sidekick;
  print_newline ();
  print_endline dalek#talk;
  print_newline ();
  dr#use_sonic_screwdriver;
  dr#attack dalek 50;
  dr#attack dalek 40;
  print_endline dalek#talk;
  dr#take_damage 50;
  print_newline ();
  print_endline dalek#talk;
  print_endline dalek#talk;
  print_endline dalek#talk;
  print_newline ();
  dr#talk;
  dr#attack dalek 150;
  print_endline dr#to_string;
  dr#nap;
  print_endline dr#to_string
