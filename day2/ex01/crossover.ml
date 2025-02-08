(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/07 15:56:58 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/08 17:20:02 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover l1 l2 = 
  let rec is_in_list value list = match list with
    | [] -> false
    | head :: tail ->  if (value = head) then true else is_in_list value tail
in 
  let rec create_new_list acc a = 
    match a with
    | [] -> acc
    | head :: tail  ->  if not (is_in_list head acc) && is_in_list head l2 then create_new_list (head :: acc) tail  
    else create_new_list acc tail in
  create_new_list [] l1


  let () =
    let test_crossover l1 l2 expected =
    let result = crossover l1 l2 in 
    if (result = expected ) then
    Printf.printf "Test passed for (%s) (%s)\n" 
      (String.concat ";" (  List.map string_of_int l1))  
      (String.concat ";" (List.map string_of_int l2))
    else 
    Printf.printf "Test failed for (%s) (%s), got (%s)\n" 
      (String.concat ";" (List.map string_of_int l1))  
      (String.concat ";" (List.map string_of_int l2))
      (String.concat ";" (List.map string_of_int result))
    in 

  test_crossover [1; 2; 3] [3; 4; 5] [3];
  test_crossover [1; 2; 3; 4] [2; 4; 6; 8] [4; 2];
  test_crossover [1; 2; 3] [4; 5; 6] [];
  test_crossover [1; 1; 2; 2] [1; 2; 3; 4] [2; 1];
  test_crossover [] [1; 2; 3] [];
  test_crossover [] [] [];
  test_crossover [1; 2; 3] [] [];
  test_crossover [1; 2; 3] [1; 2; 3] [3; 2; 1];
  test_crossover [1; 2; 3; 4; 5] [5; 4; 3; 2; 1] [5; 4; 3; 2; 1];
  test_crossover [1; 2; 3; 4; 5] [6; 7; 8; 9; 10] [];