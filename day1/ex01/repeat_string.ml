(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/29 18:26:23 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/06 16:00:29 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?(str="x") n = 
  if (n < 0) then "Error"
  else if n = 0 then ""
  else 
  let rec loop acc x= 
    if (x = 1) then acc
    else
      loop (acc ^ str) (x - 1)
    in 
    loop str n

let () = 
    print_endline (repeat_string (-1));
    print_endline (repeat_string (0));
    print_endline (repeat_string ~str:"Hello" (-1));
    print_endline (repeat_string ~str:"Hello" (0));
    print_endline (repeat_string ~str:"Hello" (3));
    print_endline (repeat_string ~str:"Hello" (10));
    print_endline (repeat_string ~str:"Hello" (1))