(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/18 17:34:07 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/20 16:44:50 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleobase : nucleotide = 
  let base = match nucleobase with
  | 'A' ->  A
  | 'T' ->  T
  | 'C' ->  C
  | 'G' ->  G
  | _ -> None 
  in

  match base with 
  | None -> ("phosphate", "deoxyribose", None)
  | b -> ("phosphate", "deoxyribose", b)

let print_nucleotide (p, d, nb) = 
  let nb_str = match nb with 
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | None -> "X"
in 
Printf.printf "(%s, %s, %s)" p d nb_str

let () =
  Printf.printf "Test generate_nucleotide 'A': ";
  print_nucleotide (generate_nucleotide 'A');
  Printf.printf "\n";

  Printf.printf "Test generate_nucleotide 'T': ";
  print_nucleotide (generate_nucleotide 'T');
  Printf.printf "\n";

  Printf.printf "Test generate_nucleotide 'C': ";
  print_nucleotide (generate_nucleotide 'C');
  Printf.printf "\n";

  Printf.printf "Test generate_nucleotide 'G': ";
  print_nucleotide (generate_nucleotide 'G');
  Printf.printf "\n";

  (* Pour un caractère invalide, la fonction renvoie ("phosphate", "deoxyribose", None) *)
  Printf.printf "Test generate_nucleotide 'X': ";
  print_nucleotide (generate_nucleotide 'X');
  Printf.printf "\n"