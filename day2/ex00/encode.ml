(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/07 12:33:16 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/20 16:49:39 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode l = 
let rec aux count acc  = function 
    | [] -> acc
    | head :: [] -> (count, head) :: acc
    | head :: next :: tail -> if (head = next ) then aux (count + 1) acc (next :: tail)
                              else (count, head) :: aux 1 acc (next :: tail) in
    aux 1 [] l

let rec print_list = function 
  | [] ->  print_string "" 
  | (a, e) :: list -> print_string  "("; print_int a; print_string  ",\'a)"; print_list list


let () =  
  if (encode []) = [] then print_string "Ok" else print_string "Failed"; print_newline ();
  print_list (encode ["lol";"lol";"Hello";"lol";"lol";"Hi";"lol"]); print_newline ();
  print_list (encode ["lol";"Hello";"Hi"]); print_newline ();
  print_list (encode [1; 1; 1]); print_newline ();
  print_list (encode [1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 18.0; 18.0]); print_newline ()
     