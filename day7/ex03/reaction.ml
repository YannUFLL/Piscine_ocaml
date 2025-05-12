(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 14:59:27 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/12 14:19:37 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction (molecules_collection_a : ((Molecule.molecule * int) list)) 
                        (molecules_collection_b : ((Molecule.molecule * int) list)) = 
  object (self)
    val _mca = molecules_collection_a
    val _mcb = molecules_collection_b
     
    method get_start = _mca
    method get_result = _mcb
    method virtual balance : reaction
    method virtual is_balanced : bool 
  end