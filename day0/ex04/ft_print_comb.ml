(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 13:44:50 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:43:58 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
  let rec increment_units x y z =
    if z <= 9 then (
      print_char (char_of_int (x + 48));
      print_char (char_of_int (y + 48));
      print_char (char_of_int (z + 48));
      if not (x = 7 && y = 8 && z = 9) then (
        print_char ',';
        print_char ' ');
      increment_units x y (z + 1))
    else ()
  in
  let rec increment_tens x y =
    if y <= 8 then (
      increment_units x y (y + 1);
      increment_tens x (y + 1))
    else ()
  in
  let rec increment_hundred x =
    if x <= 7 then (
      increment_tens x (x + 1);
      increment_hundred (x + 1))
  in
  increment_hundred 0;
  print_char '\n'

let () = ft_print_comb ()
