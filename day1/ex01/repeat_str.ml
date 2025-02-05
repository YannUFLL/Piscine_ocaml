(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_str.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/29 18:26:23 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/01/30 14:43:54 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_str ?(str="x") n = 
  let rec loop acc x= 
    if (x < 0) then "Error"
    else if (x = 0) then acc
    else
      loop (acc ^ str) (x - 1)
    in 
    loop str n

let () = 
    print_endline (repeat_str (-1));
    print_endline (repeat_str (0));
    print_endline (repeat_str ~str:"Hello" (-1));
    print_endline (repeat_str ~str:"Hello" (0));
    print_endline (repeat_str ~str:"Hello" (3));
    print_endline (repeat_str ~str:"Hello" (10))