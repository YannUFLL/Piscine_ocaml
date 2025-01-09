(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/08 17:20:21 by root              #+#    #+#             *)
(*   Updated: 2025/01/09 12:57:12 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb2 ()  = 
  let rec increment_number_2 x y = 
    if (y <= 99) then ( print_char (char_of_int((int_of_char '0')  + x / 10));
                        print_char (char_of_int((int_of_char '0')  + x mod 10));
                        print_char (' ');
                        print_char (char_of_int ((int_of_char '0') + y / 10 ));
                        print_char (char_of_int((int_of_char '0')  + y mod 10));
                        if (x <> 98 || y <> 99) then
                        (print_char (',');
                         print_char (' '));
                        increment_number_2 x (y + 1))
    else () in 
  let rec increment_number_1 x = 
    if (x < 99) then (increment_number_2 x (x + 1); increment_number_1 (x + 1)) else () in 
  increment_number_1 0;
  print_char '\n'

let () = 
    ft_print_comb2 ()