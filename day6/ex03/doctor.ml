(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 14:56:32 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:38:12 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor name age sidekick =
  object (self)
    val _name = name
    val mutable _age = age
    val _sidekick : People.people = sidekick
    val mutable _hp = 100

    method to_string =
      "name: " ^ _name ^ "; age: " ^ string_of_int _age ^ "; sidekick: ["
      ^ _sidekick#to_string ^ "]; hp: " ^ string_of_int _hp

    method talk = print_endline "Hi! I'm the Doctor!"
    method get_sidekick = _sidekick
    method set_hp hp = _hp <- hp

    method take_damage dmg =
      print_endline (_name ^ " take " ^ string_of_int dmg ^ " dmg.");
      _hp <- max 0 (_hp - dmg);
      if _hp = 0 then print_endline ("Aarghh *The doctor " ^ _name ^ " died*")
      else ()

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

    method nap =
      print_endline ("Dr " ^ _name ^ " take a nap and regenerate all his Hp");
      self#regenerate

    method private regenerate = _hp <- 100
    method attack (dalek : Dalek.dalek) dmg = dalek#take_damage dmg
    initializer print_endline ("The Doctor named " ^ _name ^ " has been created")
  end
