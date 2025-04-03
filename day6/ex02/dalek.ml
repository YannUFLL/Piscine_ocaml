(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 17:41:37 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/03 18:52:42 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek = 
  object
    val _name = "Dalek" ^ (String.make 1 (char_of_int (Random.int 26 + 91))) ^  
                (String.make 1 (char_of_int (Random.int 26 + 122))) ^ 
                (String.make 1 (char_of_int (Random.int 26 + 122)))
    val _hp = 100
    val _shield = true
    method to_string = _name ^ ": 
       hp: " ^ (string_of_int _hp) ^ "
       shield: " ^ (if _shield then "true" else "false")
    method talk = let x = Random.int 4 in 
    if (x = 0) then "Explain! Explain!\n" else
    if (x = 1) then "Exterminate! Exterminate!\n" else
    if (x = 2) then "I obey!\n" else 
                    "You are the Doctor! You are the enemy of the Daleks!\n"
    method exterminate (people: People.people) = people#take_damage 1000
    method attack_doctor (doctor: Doctor.doctor) = doctor#take_damage 10
    method die = print_endline "Emergency Temporal Shift!"
  end 