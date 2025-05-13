(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 16:00:09 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:24:53 by ydumaine         ###   ########.fr       *)
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

    method get_incomplete_results =
      let (c_total, h_total) =
      List.fold_left
        (fun (c, h) a -> (c + a#carbon_count, h + a#hydrogen_count)) (0, 0) _a_list
      in 

      let o_needed = c_total  * 2 + (h_total / 2) in
      let min_o2 = if h_total mod 2 <> 0 then (h_total / 4) + 1 else h_total / 4 in
      let max_o2 =  if o_needed / 2 = 0 then o_needed / 2 - 1 else o_needed in
       let generate_all_combinaison o = 

        let o_remaining = o - h_total / 2 in
          
        let check_triplet (co2, co, c) = 
          if ((co2 * 2) + co) <> o_remaining then false else true in
          
        let generate_triplets total_c =
        List.init (total_c + 1) (fun co2 ->
          List.init (total_c - co2 + 1) (fun co ->
            let c = total_c - co2 - co in
            (co2, co, c)
          )
          )
          |> List.concat in 
      
        let triplets = generate_triplets c_total in
        let filter_triplets = List.filter check_triplet triplets in 
        List.map  (fun (co2, co, c) ->  (((o, (if co2 <> 0 then [(((new Molecule.carbon_dioxide) :> Molecule.molecule), co2)] else []) @
                                          (if co <> 0 then [(((new Molecule.carbon_monoxide) :> Molecule.molecule), co)] else []) @
                                                  (if c <> 0 then [(((new Molecule.carbon) :> Molecule.molecule), c)] else []))))) filter_triplets in 
        (List.concat (List.init (max_o2 - min_o2 + 1) (fun i -> (generate_all_combinaison (i + min_o2)))))
        
    method balance = 
      let (c_total, h_total) =
      List.fold_left
        (fun (c, h) a -> (c + a#carbon_count, h + a#hydrogen_count)) (0, 0) _a_list in 
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
    (( new alkane_combustion ~reactants:new_reactants ~products:new_products a_list) :> Reaction.reaction) 

    method is_balanced : bool  = if _mcb != [] then true else false 

  end