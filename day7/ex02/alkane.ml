(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 13:06:13 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 13:21:03 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


class virtual alkane n = 
  object (self)
    val _name : string = match n with
    | 1 -> "Methane"
    | 2 -> "Ethane"
    | 3 -> "Propane"
    | 4 -> "Butane"
    | 5 -> "Pentane"
    | 6 -> "Hexane"
    | 7 -> "Heptane"
    | 8 -> "Octane"
    | 9 -> "Nonane"
    | 10 -> "Decane"
    | 11 -> "Undecane"
    | 12 -> "Dodecane"
    | _ -> "Unknown alkane"
    
    val _formula : string =  "C" ^ (if n > 1 then string_of_int n else "") ^ "H" ^ string_of_int (2 * n + 2)
 
    method name = _name
    method formula = _formula
    method to_string = "" ^ self#name ^ ": " ^ self#formula
    method compare (other : alkane) = 
      other#formula = self#formula &&
      other#name = self#name
  end

class methane =
object 
  inherit alkane 1
end

class ethane = 
object 
  inherit alkane 2
end 

class octane = 
object 
  inherit alkane 8 
end