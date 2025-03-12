(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/10 18:44:35 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/11 13:19:19 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let examples_of_file fileName =

let parse_csv_line line = 
  String.split_on_char ',' line in

let remove_last lst =
  match List.rev lst with
  | [] -> failwith "empty list"
  | _ :: rev_tail -> List.rev rev_tail in

let channel = 
  try open_in fileName 
  with 
    | _ -> failwith "Error opening file" in
let final_list = [] in
let acc = ref [] in
try 
let first_line = input_line channel in
let parsed_first_line = parse_csv_line first_line in
let vector_length = List.length parsed_first_line in
let last_char = List.nth parsed_first_line (vector_length - 1) in
let vector_first_list = remove_last parsed_first_line in
let float_first_line = List.map float_of_string vector_first_list in
let first_tab =  Array.make (vector_length - 1) 0.0 in
let add_element_to_tab index element = Array.set first_tab index element in
List.iteri add_element_to_tab float_first_line;
let list = (first_tab, last_char) :: final_list in
acc := list;
while true do
  let line = input_line channel in
  let parsed_line = parse_csv_line line in
  let vector_length = List.length parsed_line in
  let last_char = List.nth parsed_line (vector_length - 1) in
  let vector_list = remove_last parsed_line in
  let float_line = List.map float_of_string vector_list in
  let tab =  Array.make (vector_length - 1) 0.0 in
  let add_element_to_tab index element = Array.set tab index element in
  List.iteri add_element_to_tab float_line;
  acc :=  (tab, last_char) :: !acc done;
  !acc
with End_of_file -> (close_in channel; List.rev !acc;)

let print_list list = 
  for i = 0 to ((List.length list) - 1) do
    let (array, letter) = List.nth list i in
    print_string "([|"; 
    for i = 0 to ((Array.length array) - 1)do 
      print_float array.(i);
      print_string "; "; done;
    print_string "|], \"";
    print_string letter;
    print_string "\")";
    print_newline (); 
  done
      
  

let () = 
  let fileName = try 
  Sys.argv.(1)
  with 
  | _ -> failwith "Error: you must provide n valid argument" in 
  print_list (examples_of_file fileName)
      


  
  