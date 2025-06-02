(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/23 17:26:55 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/02 11:43:14 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual molecule name (atoms : Atom.atom list) =
  object (self)
    val _name : string = name
    val _atoms : Atom.atom list = atoms

    val _formula : string =
      let rec add_to_list (a : Atom.atom) mol =
        match mol with
        | (x, q) :: tail ->
            if x = a#symbol then (x, q + 1) :: tail
            else if
              a#symbol < x || a#symbol = "C"
              || (a#symbol = "H" && a#symbol != "C")
            then (a#symbol, 1) :: (x, q) :: tail
            else (x, q) :: add_to_list a tail
        | [] -> (a#symbol, 1) :: []
      in
      let rec generate_string mol =
        match mol with
        | (a, q) :: tail ->
            a ^ (if q > 1 then string_of_int q else "") ^ generate_string tail
        | [] -> ""
      in
      let mol = List.fold_right add_to_list atoms [] in
      generate_string mol

    method name = _name
    method formula = _formula
    method to_string = "" ^ _name ^ ": " ^ _formula

    method equals (other : molecule) =
      other#formula = self#formula && other#name = self#name
  end

class water =
  object
    inherit
      molecule "Water" [ new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen ]
  end

class carbon =
  object
    inherit molecule "Carbon" [ new Atom.carbon ]
  end

class carbon_dioxide =
  object
    inherit
      molecule
        "Carbon Dioxide"
        [ new Atom.carbon; new Atom.oxygen; new Atom.oxygen ]
  end

class carbon_monoxide =
  object
    inherit molecule "Carbon Monoxide" [ new Atom.carbon; new Atom.oxygen ]
  end

class methane =
  object
    inherit
      molecule
        "Methane"
        [
          new Atom.carbon;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
        ]
  end

class ethanol =
  object
    inherit
      molecule
        "Ethanol"
        [
          new Atom.carbon;
          new Atom.carbon;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.oxygen;
        ]
  end

class glucose =
  object
    inherit
      molecule
        "Glucose"
        [
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
        ]
  end

class trinitrotoluene =
  object
    inherit
      molecule
        "trinitrotoluene"
        (List.init 7 (fun _ -> new Atom.carbon)
        @ List.init 5 (fun _ -> new Atom.hydrogen)
        @ List.init 3 (fun _ -> new Atom.nitrogen)
        @ List.init 6 (fun _ -> new Atom.oxygen))
  end

class dioxygen =
  object
    inherit molecule "Dioxygen" [ new Atom.oxygen; new Atom.oxygen ]
  end
