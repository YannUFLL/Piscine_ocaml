(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/04 12:44:15 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:38:14 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let dalek_army = new Army.army in
  print_int dalek_army#get_army_size;
  print_newline ();
  dalek_army#add (new Dalek.dalek);
  print_int dalek_army#get_army_size;
  print_newline ();
  dalek_army#add (new Dalek.dalek);
  print_int dalek_army#get_army_size;
  print_newline ();
  dalek_army#add (new Dalek.dalek);
  print_int dalek_army#get_army_size;
  print_newline ();
  dalek_army#delete;
  print_int dalek_army#get_army_size;
  print_newline ();
  let people_army = new Army.army in
  people_army#add (new People.people "yann");
  people_army#add (new People.people "bob");
  print_endline people_army#get_member#to_string;
  print_endline people_army#get_member#to_string;
  let dr_army = new Army.army in
  dr_army#add (new Doctor.doctor "Geobra" 18 (new People.people "Dimitri"));
  dr_army#delete;
  print_endline people_army#get_member#to_string
