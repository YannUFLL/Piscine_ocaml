(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 18:11:22 by root              #+#    #+#             *)
(*   Updated: 2025/01/07 18:43:54 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_string_all predicate str = 
  let length = String.length str in
  let rec test_char x =
    if (x >= 0) then
      (predicate (String.get str x) && test_char (x - 1))
    else 
      true
  in 
  test_char (length - 1)

let is_digit c = c >= '0' && c <= '9'

let () = 
  if (ft_string_all is_digit "sqsewmegmeogjeo") then print_endline("true")
else print_endline("false");
  if (ft_string_all is_digit "sqsewm5gmeogjeo") then print_endline("true")
else print_endline("false");
  if (ft_string_all is_digit "565565") then print_endline("true")
else print_endline("false");
