(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   life.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 14:10:55 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/20 16:41:35 by ydumaine         ###   ########.fr       *)
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

let generate_helix_str (str : string) =
  if String.length str <= 0 then []
  else 
    let length = String.length str in 
    let rec aux acc i = 
      if i = length then acc
      else aux (generate_nucleotide(String.get str i) :: acc) (i + 1) in
    reverse_list (aux [] 0)

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

let decode_arn arn =
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

let rec triplets_to_string (triplets : (nucleobase * nucleobase * nucleobase) list) : string =
  let convert_triplet (a, b, c) : string =
    let convert_nucleobase n = match n with
      | A -> "A"
      | T -> "T"
      | C -> "C"
      | G -> "G"
      | U -> "U"
      | None -> "X" in
    "(" ^ (convert_nucleobase a) ^ ", " ^ (convert_nucleobase b) ^ ", " ^ (convert_nucleobase c) ^ ")" in

  match triplets with
  | [] -> ""
  | head :: tail -> (convert_triplet head) ^ " " ^ (triplets_to_string tail)

let life (str : string) = 
  
  print_newline ();
  print_string "===== Inital ADN Code =====\n";
  print_string str; 

  print_newline ();
  print_string "===== Generate Helix based on ADN sequence =====\n";
  let helix = generate_helix_str str in
  print_string (helix_to_string helix); 

  print_newline ();
  print_string "===== Creation of ARN based on Helix =====\n";
  let rna = generate_rna helix in
  print_string (rna_to_string rna); 

  print_newline ();
  print_string "===== Ribosome : Decode ARN and fabricate protein =====\n";
  let protein = decode_arn rna in 
  print_string (string_of_protein protein)
  
  
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


let () = 
  life "ATCGTACGTA";
  print_newline ();

  life "GATTACA";
  print_newline ();

  life "CGTAGCTAGCTAGCTA";
  print_newline ();

  life "TGCATGCATGCATGCA";
  print_newline ();

  life "AATTCCGG";
  print_newline ();
