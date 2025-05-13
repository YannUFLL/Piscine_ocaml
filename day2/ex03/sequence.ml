(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/17 16:05:40 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:42:10 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let reverse_list list =
  let rec aux acc list =
    match list with [] -> acc | head :: tail -> aux (head :: acc) tail
  in
  aux [] list

let rec string_of_int_list list =
  let rec create_string_number x =
    let get_figure y =
      match y with
      | 0 -> "0"
      | 1 -> "1"
      | 2 -> "2"
      | 3 -> "3"
      | 4 -> "4"
      | 5 -> "5"
      | 6 -> "6"
      | 7 -> "7"
      | 8 -> "8"
      | 9 -> "9"
      | _ -> "Error"
    in
    match x with
    | x when x >= 10 -> create_string_number (x / 10) ^ get_figure (x mod 10)
    | x -> get_figure x
  in

  let rec loop list =
    match list with
    | [] -> ""
    | head :: tail -> create_string_number head ^ loop tail
  in
  loop list

let sequence n =
  if n <= 0 then ""
  else if n = 1 then "1"
  else
    let rec count_occurence count (l : int list) =
      match l with
      | y :: ys :: rest when y = ys ->
          count_occurence (count + 1) (ys :: rest)
          (* count the number of occurence*)
      | head :: rest -> (count, rest) (* return the rest of the list*)
      | head -> (count, [])
    in

    let rec create_list list =
      match list with
      | [] -> []
      | first :: _ ->
          let count, rest = count_occurence 1 list in
          count :: first :: create_list rest
    in

    let rec search_n_list i old_list =
      if i = n then old_list else search_n_list (i + 1) (create_list old_list)
    in
    string_of_int_list (search_n_list 1 [ 1 ])

let () =
  print_endline (sequence (-1));
  print_endline (sequence 0);
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 7);
  print_endline (sequence 20);
  print_endline (sequence 40)
