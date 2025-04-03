(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 13:01:30 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/03 14:07:34 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people name = 
  object
    initializer print_endline "The people is appear in the world"
    val _hp = 50
    val _name = name
    method to_string = "name: " ^ _name ^ " age: " ^ (string_of_int _hp)
    method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")
    method die = print_endline ("Aaaarghh!")
  end
   