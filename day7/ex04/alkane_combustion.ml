(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 16:00:09 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:24:59 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)



class alkane_combustion 
                        ?(reactants : (Molecule.molecule * int) list  = [])  
                        ?(products : (Molecule.molecule * int) list  = []) 
                        (a_list: (Alkane.alkane list))  =
  object (self)
    val _a_list = a_list
    inherit Reaction.reaction reactants products
     
    method get_start = 
      if _mcb = [] then 
        failwith "Reaction is not balanced"
      else 
        _mca
    method get_result = 
      if _mcb = [] then 
        failwith "Reaction is not balanced"
    else _mcb
  
    method balance = 
      let (c_total, h_total) =
      List.fold_left
        (fun (c, h) a -> (c + a#carbon_count, h + a#hydrogen_count)) (0, 0) _a_list
    in 
      let o_needed = c_total  * 2 + (h_total / 2) in
      let scale =  if o_needed mod 2 <> 0 then 2 else 1 in

    let rec add_to_list (a : Alkane.alkane) acc =
      match acc with
      | (x, q) :: t when x#compare (a :> Molecule.molecule) -> (x, q + scale) :: t
      | (x, q) :: t -> (x, q) :: add_to_list a t
      | [] -> [((a :> Molecule.molecule), scale)]
        in
        let new_reactants = ((List.fold_right add_to_list _a_list []) :> ((Molecule.molecule * int) list)) @ [(new Molecule.dioxygen, scale * o_needed / 2 )] in
        
        let new_products = [(new Molecule.carbon_dioxide, c_total * scale); 
                        (new Molecule.water, h_total / 2 * scale);
          ] in 
            new alkane_combustion ~reactants:new_reactants ~products:new_products a_list

      
    method is_balanced : bool  = if _mcb != [] then true else false 
  end