(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 17:41:37 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/04 14:43:28 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek = 
  object(self)
    val _name = "Dalek" ^ (String.make 1 (char_of_int (Random.int 26 + 65))) ^  
                (String.make 1 (char_of_int (Random.int 26 + 97))) ^ 
                (String.make 1 (char_of_int (Random.int 26 + 97)))
    val mutable _hp = 100
    val _shield = true
    method to_string = _name ^ ": 
       hp: " ^ (string_of_int _hp) ^ "
       shield: " ^ (if _shield then "true" else "false")
    method talk = let x = Random.int 4 in 
    if (x = 0) then "Explain! Explain!" else
    if (x = 1) then "Exterminate! Exterminate!" else
    if (x = 2) then "I obey!" else 
                    "You are the Doctor! You are the enemy of the Daleks!"
    method exterminate (people: People.people) = people#take_damage 1000
    method die = print_endline "Emergency Temporal Shift!"
    method is_still_alive = if _hp > 0  then true else false 
    method take_damage dmg = 
      print_endline (_name ^ " take " ^ string_of_int (if _shield then dmg / 2 else dmg)^ " dmg.");
      _hp <- (max 0 (_hp - (if _shield then dmg / 2 else dmg))); 
      if _hp = 0 then self#die
  end 