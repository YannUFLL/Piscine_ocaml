(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/29 18:26:23 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:43:34 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =
  let rec loop acc x =
    if x < 0 then "Error" else if x = 0 then acc else loop (acc ^ "x") (x - 1)
  in
  loop "" n

let () =
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5)
