(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 14:56:32 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:37:33 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor name age sidekick =
  object
    val _name = name
    val mutable _age = age
    val _sidekick : People.people = sidekick
    val mutable _hp = 100

    method to_string =
      "name: " ^ _name ^ "; age: " ^ string_of_int _age ^ "; sidekick: ["
      ^ _sidekick#to_string ^ "]; hp: " ^ string_of_int _hp

    method talk = print_endline "Hi! I'm the Doctor!"

    method travel_in_time start arrival =
      _age <-
        (let new_age = arrival - start + _age in
         if new_age > 0 then new_age else 0);
      print_endline
        "            ___         \n\
        \    _______(_@_)_______\n\
        \    | POLICE      BOX |\n\
        \    |_________________|\n\
        \     | _____ | _____ |\n\
        \     | |###| | |###| |\n\
        \     | |###| | |###| | \n\
        \     | _____ | _____ |\n\
        \     | || || | || || |\n\
        \     | ||_|| | ||_|| |\n\
        \     | _____ |$_____ |\n\
        \     | || || | || || |\n\
        \     | ||_|| | ||_|| |\n\
        \     | _____ | _____ |\n\
        \     | || || | || || | \n\
        \     | ||_|| | ||_|| | \n\
        \     |       |       | \n\
        \     *****************"

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenerate = _hp <- 100
    initializer print_endline ("The Doctor named " ^ _name ^ " has been created")
  end
