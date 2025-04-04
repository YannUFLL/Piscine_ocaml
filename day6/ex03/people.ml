(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 13:01:30 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/04 14:42:42 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people name = 
  object(self)
    val mutable _hp = 100
    val _name = name
    method to_string = "name: " ^ _name ^ " age: " ^ (string_of_int _hp)
    method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")
    method die = print_endline ("Aaaarghh!")
    method is_still_alive = if _hp > 0  then true else false 
    method take_damage dmg = 
      print_endline (_name ^ " take " ^ string_of_int dmg ^ " dmg.");
      _hp <- (max 0 (_hp - dmg)); 
      if _hp = 0 then self#die
    initializer print_endline ("A people named " ^ _name ^  " appear in the world")
  end
   