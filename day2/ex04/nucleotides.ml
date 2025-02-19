(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/18 17:34:07 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/19 11:48:19 by ydumaine         ###   ########.fr       *)
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