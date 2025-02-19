(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/19 11:47:36 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/19 12:29:00 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleobase : nucleotide = 
  let base = match nucleobase with
  | 'A' -> Some A
  | 'T' -> Some T
  | 'C' -> Some C
  | 'G' -> Some G
  | _ -> None 
  in

  match base with 
  | None -> ("phosphate", "deoxyribose", A)
  | Some b -> ("phosphate", "deoxyribose", b)
  
  type helix = nucleotide list

let generate_helix n : helix =
  if n <= 0 then []
  else 
    let generate_nucleobase_char = 
      let nucleobase = Random.int 4 in
      match nucleobase with 
      | 0 -> 'A'
      | 1 -> 'T'
      | 2 -> 'G'
      | 3 -> 'C'   
      | _ -> 'X' in
    let rec aux acc i = 
      if i = n then acc
      else aux (generate_nucleotide(generate_nucleobase_char) :: acc) (i + 1) in
    aux [] 0