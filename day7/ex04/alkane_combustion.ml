(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 16:00:09 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/24 19:21:49 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)



class alkane_combustion (a_list: (Alkane.alkane list)) =
  object (self)
    

    val _a_list = a_list
    inherit Reaction.reaction  
    (let rec add_to_list (a: Alkane.alkane) col = match col with
    | (x, q) :: tail -> if x#compare a then (x, q + 1) :: tail
                        else (x, q) :: add_to_list a tail 
    | [] -> (a, 1) :: [] in 
    List.fold_right add_to_list a_list [])
    []
     
    method get_start = 
      if _mcb = [] then 
        failwith "Reaction is not balanced"
      else 
        _mca
    method get_result = 
      if _mcb = [] then 
        failwith "Reaction is not balanced"
    else _mcb
    method balance = new alkane_combustion (

   
    let number_of_atoms =  List.fold_left (fun (c, h) (a) -> (c + a#carbon_count, h + a#hydrogen_count)) (0, 0) _a_list in
    let (c_count, h_count) = number_of_atoms in 
    _mca @ ((new Molecule.dioxygen, (h_count / 2) :: [])

    ) []
    method is_balanced : bool  = if _mcb != [] then true else false 
  end