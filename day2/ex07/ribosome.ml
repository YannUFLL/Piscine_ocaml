(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ribosome.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/19 16:50:14 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/21 12:08:46 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let reverse_list list = 
  let rec aux acc list = match list with
  | [] -> acc
  | head :: tail -> aux (head :: acc) tail in
  aux [] list

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleobase : nucleotide = 
  let base = match nucleobase with
  | 'A' -> Some A
  | 'T' -> Some T
  | 'C' -> Some C
  | 'G' -> Some G
  | 'U' -> Some U
  | _ -> None 
  in

  match base with 
  | None -> ("phosphate", "deoxyribose", None)
  | Some b -> ("phosphate", "deoxyribose", b)
  
  type helix = nucleotide list

let generate_helix n : helix =
  if n <= 0 then []
  else 
    let generate_nucleobase_char () = 
      let nucleobase = Random.int 4 in
      match nucleobase with 
      | 0 -> 'A'
      | 1 -> 'T'
      | 2 -> 'G'
      | 3 -> 'C'   
      | _ -> 'X' in
    let rec aux acc i = 
      if i = n then acc
      else aux (generate_nucleotide(generate_nucleobase_char ()) :: acc) (i + 1) in
    aux [] 0

let rec helix_to_string (h : helix) : string = 
  let convert_nucleobase n = match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G" 
    | U -> "U"
    | None -> "X" in 

  match h with
  | [] -> ""
  | head :: tail -> let (a, b, base) = head in
                    convert_nucleobase(base) ^ helix_to_string tail 
  
let rec complementary_helix (h : helix) : helix = 
   let get_associated_base b = match b with
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C' 
      | U -> 'X'
      | None -> 'X' in
    
  let rec aux acc h = match h with 
    | [] -> acc
    | head :: tail -> 
        let (_, _, base) = head in 
        aux (generate_nucleotide (get_associated_base (base)) :: acc) tail in
    reverse_list (aux [] h)



type rna = nucleobase list

let rec generate_rna (h : helix) : rna = 
  let get_associated_base b = match b with
     | A -> U
     | T -> A
     | C -> G
     | G -> C 
     | U -> U
     | None -> None in
   
 let rec aux acc h = match h with 
   | [] -> acc
   | head :: tail -> 
       let (_, _, base) = head in 
       aux (get_associated_base (base) :: acc) tail in
   reverse_list (aux [] h)

let generate_base_triplets (rna : rna) = 
  let rec aux acc rna = match rna with
  | (first) :: (second) :: (third) :: rest -> aux ((first, second, third) :: acc) rest
  | _ -> acc in
  reverse_list (aux [] rna)

type aminoacid = 
  | Stop
  | Ala 
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln 
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

let rec string_of_protein protein : string =
  let string_of_aminoacid aminoacid = match aminoacid with
    | Stop -> "Stop"
    | Ala -> "Alanine"
    | Arg -> "Arginine"
    | Asn -> "Asparagine"
    | Asp -> "Aspartique"
    | Cys -> "Cysteine"
    | Gln -> "Glutamine"
    | Glu -> "Glutamique"
    | Gly -> "Glycine"
    | His -> "Histidine"
    | Ile -> "Isoleucine"
    | Leu -> "Leucine"
    | Lys -> "Lysine"
    | Met -> "Methionine"
    | Phe -> "Phenylalanine"
    | Pro -> "Proline"
    | Ser -> "Serine"
    | Thr -> "Threonine"
    | Trp -> "Tryptophan"
    | Tyr -> "Tyrosine"
    | Val -> "Valine" in

  let rec create_string (acc : string) protein_list =  match protein_list with
  | head :: tail -> let rec aux (acc : string) protein_list = 
    match protein_list with
    | [] -> acc
    | head :: tail-> aux (head ^ " - " ^ acc) tail in
    aux head tail
  | [] -> "" in 
     
    
  let rec aux acc protein = match protein with 
  | [] -> acc
  | aminoacid :: rest -> aux ((string_of_aminoacid aminoacid) :: acc) rest in
    create_string "" (aux [] protein)

let decode_arn arn : protein =
  let triplets = generate_base_triplets (arn) in
  let rec aux acc triplets = match triplets with
  | ((U,A,A) | (U,A,G) | (U,G,A)) :: rest -> acc
  | ((G,C,A) | (G,C,C) | (G,C,G) | (G,C,U)) :: rest -> aux (Ala :: acc) rest
  | ((A,G,A) | (A,G,G) | (C,G,A) | (C,G,C) | (C,G,G) | (C,G,U)) :: rest -> aux (Arg :: acc) rest
  | ((A,A,C) | (A,A,U)) :: rest -> aux (Asn :: acc) rest
  | ((G,A,C) | (G,A,U)) :: rest -> aux (Asp :: acc) rest
  | ((U,G,C) | (U,G,U)) :: rest -> aux (Cys :: acc) rest
  | ((C,A,A) | (C,A,G)) :: rest -> aux (Gln :: acc) rest
  | ((G,A,A) | (G,A,G)) :: rest -> aux (Glu :: acc) rest
  | ((G,G,A) | (G,G,C) | (G,G,G) | (G,G,U)) :: rest -> aux (Gly :: acc) rest
  | ((C,A,C) | (C,A,U)) :: rest -> aux (His :: acc) rest
  | ((A,U,A) | (A,U,C) | (A,U,U)) :: rest -> aux (Ile :: acc) rest
  | ((C,U,A) | (C,U,C) | (C,U,G) | (C,U,U) | (U,U,A) | (U,U,G)) :: rest -> aux (Leu :: acc) rest
  | ((A,A,A) | (A,A,G)) :: rest -> aux (Lys :: acc) rest
  | (A,U,G) :: rest -> aux (Met :: acc) rest
  | ((U,U,C) | (U,U,U)) :: rest -> aux (Phe :: acc) rest
  | ((C,C,C) | (C,C,A) | (C,C,G) | (C,C,U)) :: rest -> aux (Pro :: acc) rest
  | ((U,C,A) | (U,C,C) | (U,C,G) | (U,C,U) | (A,G,U) | (A,G,C)) :: rest -> aux (Ser :: acc) rest
  | ((A,C,A) | (A,C,C) | (A,C,G) | (A,C,U)) :: rest -> aux (Thr :: acc) rest
  | (U,G,G) :: rest -> aux (Trp :: acc) rest
  | ((U,A,C) | (U,A,U)) :: rest -> aux (Tyr :: acc) rest
  | ((G,U,A) | (G,U,C) | (G,U,G) | (G,U,U)) :: rest -> aux (Val :: acc) rest
  | _ :: rest -> aux acc rest
  | [] -> acc in 
  reverse_list (aux [] triplets)

  
    (* Function exclusively for testing purpose*)

let print_nucleotide (p, d, nb) = 
  let nb_str = match nb with 
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> "X" in 
Printf.printf "(%s, %s, %s)" p d nb_str

let rec rna_to_string (h : rna) : string = 
  let convert_nucleobase n = match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G" 
    | U -> "U"
    | None -> "X" in 

  match h with
  | [] -> ""
  | base :: tail -> convert_nucleobase(base) ^ rna_to_string tail 

let () = 
  (* Test with 10 nucleotides *)
  let helix_10 = generate_helix 10 in
  print_string "===== Test with helix of 10 nucleotides =====\n";
  print_string (helix_to_string helix_10); print_newline ();
  let rna_10 = generate_rna helix_10 in  
  print_string (rna_to_string rna_10); print_newline ();
  let protein_10 = decode_arn rna_10 in
  print_string (string_of_protein protein_10); print_newline ();  

  print_newline ();
  print_string "===== Test with helix of 100 nucleotides =====\n";

  (* Test with 100 nucleotides *)
  let helix_100 = generate_helix 100 in
  print_string (helix_to_string helix_100); print_newline ();
  let rna_100 = generate_rna helix_100 in  
  print_string (rna_to_string rna_100); print_newline ();
  let protein_100 = decode_arn rna_100 in
  print_string (string_of_protein protein_100); print_newline ();

  (* Test with an empty list *)
  print_newline ();
  print_string "===== Test with an empty list =====\n";
  print_string (string_of_protein ([])); print_newline ();

  (* Test with a predefined list of amino acids *)
  print_newline ();
  print_string "===== Test with a predefined protein =====\n";
  print_string (string_of_protein ([Ala; Thr; Ser; Val; Met; Lys])); print_newline ();

  (* Special test: DNA containing only T *)
  print_newline ();
  print_string "===== Test with a helix containing only A (should give U in RNA) =====\n";
  let helix_only_t = List.init 10 (fun _ -> ("phosphate", "deoxyribose", A)) in
  print_string (helix_to_string helix_only_t); print_newline ();
  let rna_only_t = generate_rna helix_only_t in  
  print_string (rna_to_string rna_only_t); print_newline ();

  (* Special test: Check a STOP codon *)
  print_newline ();
  print_string "===== Test with an RNA containing a STOP codon at the start =====\n";
  let rna_stop = [U;A;G;C;A;U;A;A;C;G;A;C] in
  print_string (rna_to_string rna_stop); print_newline ();
  let stop_protein = decode_arn rna_stop in
  print_string (string_of_protein stop_protein); print_newline ();

  (* Special test: Check a STOP codon inside rna *)
  print_newline ();
  print_string "===== Test with an RNA containing a STOP codon at the middle of it =====\n";
  let rna_stop = [C;C;A;A;A;G;G;A;C;U;A;G;C;A;U;A;A;C;G;A;C] in
  print_string (rna_to_string rna_stop); print_newline ();
  let stop_protein = decode_arn rna_stop in
  print_string (string_of_protein stop_protein); print_newline ();