(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 16:20:42 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/07 18:12:23 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
let fileName = 
try 
  Sys.argv.(1)
with 
  | _ -> failwith "Error: you must provide n valid argument" in
let channel = 
try open_in fileName with | _ -> failwith "Error opening file" in
let tab = ref (Array.make 10 "") in 
let number_of_elements = ref 0 in
try 
while true do 
  let line = input_line channel in
  if !number_of_elements > (Array.length !tab) then (
    let new_tab = Array.make ((Array.length !tab) + 10) ""in
    Array.blit !tab 0 new_tab 0 (Array.length !tab);
    tab := new_tab
  ) else (); 
    !tab.(!number_of_elements) <- line;
  incr number_of_elements done with 
  | End_of_file ->  Random.self_init ();
                    let index = Random.int (Array.length !tab) in
                    (print_string !tab.(index); print_newline ())
  | _ -> failwith "Error whith file"