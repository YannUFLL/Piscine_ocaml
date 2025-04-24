(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/23 16:55:22 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 14:55:03 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual atom name symbol atomic_number =
object (this)
  val _name = name
  val _symbol = symbol 
  val _atomic_number = atomic_number
  method name = _name
  method symbol = _symbol
  method atomic_number = _atomic_number
  method to_string = 
    "This is an atom of " ^ this#name ^ " (" ^ this#symbol ^ "), with atomic number " ^ string_of_int this#atomic_number ^ "."
  method equal (other : atom) =
    this#name = other#name &&
    this#symbol = other#symbol &&
    this#atomic_number = other#atomic_number
end

class hydrogen = 
object 
  inherit atom "hydrogen" "H" 1
end 

class helium =
object 
  inherit atom "helium" "He" 2
end

class lithium =
object 
  inherit atom "lithium" "Li" 3
end

class carbon =
object 
  inherit atom "carbon" "C" 6
end

class iron =
object 
  inherit atom "iron" "Fe" 26
end

class oxygen =
object 
  inherit atom "oxygen" "O" 8
end

class nitrogen =
object 
  inherit atom "nitrogen" "N" 7
end
