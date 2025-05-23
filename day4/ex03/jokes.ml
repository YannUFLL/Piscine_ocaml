(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 16:20:42 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:40:06 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let fileName =
    try Sys.argv.(1)
    with _ -> failwith "Error: you must provide n valid argument"
  in
  let channel =
    try open_in fileName with _ -> failwith "Error opening file"
  in
  let tab = ref (Array.make 10 "") in
  let index_element = ref 0 in
  try
    while true do
      let line = input_line channel in
      if !index_element >= Array.length !tab then (
        let new_tab = Array.make (Array.length !tab + 10) "" in
        Array.blit !tab 0 new_tab 0 (Array.length !tab);
        tab := new_tab)
      else ();
      if line <> "" then
        !tab.(!index_element) <- !tab.(!index_element) ^ line ^ "\n"
      else incr index_element
    done
  with
  | End_of_file ->
      Random.self_init ();
      let index = Random.int (!index_element + 1) in
      print_string !tab.(index)
  | _ -> failwith "Error whith file"
