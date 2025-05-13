(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 13:06:13 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:27:11 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual alkane (n : int) =
  let carbon_atoms = List.init n (fun _ -> new Atom.carbon) in
  let hydrogen_atoms = List.init ((2 * n) + 2) (fun _ -> new Atom.hydrogen) in
  let all_atoms = carbon_atoms @ hydrogen_atoms in
  let name =
    match n with
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
  in
  object (self)
    inherit Molecule.molecule name all_atoms
    method carbon_count = n
    method hydrogen_count = (2 * n) + 2
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
