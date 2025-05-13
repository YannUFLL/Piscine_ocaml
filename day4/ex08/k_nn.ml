(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ml                                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 12:35:50 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/11 13:13:45 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = float array * string

let examples_of_file fileName =
  let parse_csv_line line = String.split_on_char ',' line in

  let remove_last lst =
    match List.rev lst with
    | [] -> failwith "empty list"
    | _ :: rev_tail -> List.rev rev_tail
  in

  let channel =
    try open_in fileName with _ -> failwith "Error opening file"
  in
  let final_list = [] in
  let acc = ref [] in
  try
    let first_line = input_line channel in
    let parsed_first_line = parse_csv_line first_line in
    let vector_length = List.length parsed_first_line in
    let last_char = List.nth parsed_first_line (vector_length - 1) in
    let vector_first_list = remove_last parsed_first_line in
    let float_first_line = List.map float_of_string vector_first_list in
    let first_tab = Array.make (vector_length - 1) 0.0 in
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
      let tab = Array.make (vector_length - 1) 0.0 in
      let add_element_to_tab index element = Array.set tab index element in
      List.iteri add_element_to_tab float_line;
      acc := (tab, last_char) :: !acc
    done;
    !acc
  with End_of_file ->
    close_in channel;
    List.rev !acc

let print_list list =
  for i = 0 to List.length list - 1 do
    let array, letter = List.nth list i in
    print_string "([|";
    for i = 0 to Array.length array - 1 do
      print_float array.(i);
      print_string "; "
    done;
    print_string "|], \"";
    print_string letter;
    print_string "\")";
    print_newline ()
  done

let select_type list_nei k =
  let frequency = ref [] in
  for i = 0 to k - 1 do
    let _, d, label = !list_nei.(i) in
    let exists =
      try
        let _ = List.find (fun (l, _, _) -> l = label) !frequency in
        true
      with Not_found -> false
    in
    if exists then
      frequency :=
        List.map
          (fun (l, count, total_value) ->
            if l = label then (l, count + 1, total_value +. d)
            else (l, count, total_value))
          !frequency
    else frequency := (label, 1, d) :: !frequency
  done;
  let best_label, _, _ =
    List.fold_left
      (fun (curr_label, curr_count, curr_d) (l, count, d) ->
        if count > curr_count || (curr_count = count && d < curr_d) then
          (l, count, d)
        else (curr_label, curr_count, curr_d))
      ("", 0, max_float) !frequency
  in
  best_label

let k_nn radar_list k query_radar =
  if k > List.length radar_list then
    invalid_arg "k cannot exceed the total of training exemple";
  let eu_dist a b =
    if Array.length a <= 0 then
      raise (Invalid_argument "Array length must be > 0")
    else
      let acc = ref 0.0 in
      for i = 0 to Array.length a - 1 do
        acc := !acc +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
      done;
      sqrt !acc
  in
  let closest_nei = ref (Array.make k (-1, -1.0, "")) in
  let updated = ref false in
  let array_filling = ref 0 in
  for i = 0 to List.length radar_list - 1 do
    updated := false;
    for j = 0 to k - 1 do
      if not !updated then (
        let current_dist = eu_dist (fst (List.nth radar_list i)) query_radar in
        let stored_index, sorted_dist, _ = !closest_nei.(j) in
        if
          j >= !array_filling
          || (current_dist < sorted_dist && !array_filling = k)
        then (
          if !array_filling != k then incr array_filling;
          !closest_nei.(j) <- (i, current_dist, snd (List.nth radar_list i));
          updated := true))
      else ()
    done
  done;
  select_type closest_nei k

let () =
  let train_file = "ionosphere.train.csv" in
  let test_file = "ionosphere.test.csv" in
  let train_examples = examples_of_file train_file in
  let test_examples = examples_of_file test_file in

  Printf.printf "Running classification on test examples...\n";

  let total = List.length test_examples in
  let correct = ref 0 in
  (* let predicted = k_nn train_examples 4 (fst (List.nth test_examples 1)) in *)

  for i = 1 to 20 do
    correct := 0;
    List.iteri
      (fun idx (features, true_label) ->
        let predicted = k_nn train_examples i features in
        if predicted = true_label then correct := !correct + 1
        else
          (* Printf.printf ("Test Example %d: True Label: %s, Predicted: %s\n")
        idx true_label predicted *)
          ())
      test_examples;
    Printf.printf "k = %d accuracy: %.2f%%\n" i
      (float_of_int !correct /. float_of_int total *. 100.0)
  done
