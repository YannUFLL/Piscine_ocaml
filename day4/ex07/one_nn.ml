(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 12:35:50 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/11 13:13:45 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


type radar = float array * string

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

let one_nn radar_list query_radar = 
  let eu_dist a b = 
    if Array.length a <= 0 then 
      raise (Invalid_argument "Array length must be > 0")
    else (
      let acc = ref 0.0 in
      for i = 0 to (Array.length a - 1) do
        acc := !acc +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
      done;
      sqrt !acc) in
  let closest_eu_dist = ref (-1.0) in
  let closest_nei = ref (-1) in
  for i = 0 to List.length radar_list - 1 do
    let current_dist = eu_dist  (fst (List.nth radar_list i)) query_radar in
    if current_dist < !closest_eu_dist || !closest_nei = -1 
      then begin 
        closest_eu_dist := current_dist;
        closest_nei := i;
      end 
    else () done;
  snd (List.nth radar_list !closest_nei)
        
 let () =
    let train_file = "ionosphere.train.csv" in
    let test_file  = "ionosphere.test.csv" in
    let train_examples = examples_of_file train_file in
    let test_examples  = examples_of_file test_file in
  
    Printf.printf "Running classification on test examples...\n";
  
    let total = List.length test_examples in
    let correct = ref 0 in
  
    List.iteri (fun idx (features, true_label) ->
      let predicted = one_nn train_examples features in
      if predicted = true_label then correct := !correct + 1;
      Printf.printf "Test Example %d: True Label: %s, Predicted: %s\n"
        idx true_label predicted
    ) test_examples;
  
    Printf.printf "Accuracy: %.2f%%\n" ((float_of_int !correct /. float_of_int total) *. 100.0)