(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 18:45:10 by root              #+#    #+#             *)
(*   Updated: 2025/01/08 19:03:07 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str = 
  let length = String.length str in
  if (length = 0)
    then true
  else if ((length mod 2 <> 1))
    then false
  else 
  let rec ft_compare_char x last_char = 
    if (x <= last_char / 2 &&  String.get str x = String.get str (last_char - x))
      then (ft_compare_char (x + 1) last_char && true)
    else 
      true in
  ft_compare_char 0 (length - 1)

 
let () = 
  if (ft_is_palindrome  "coucou") then print_endline("true")
  else print_endline("false");
    if (ft_is_palindrome  "maram") then print_endline("true")
  else print_endline("false");
    if (ft_is_palindrome  "wesh") then print_endline("true")
  else print_endline("false");
    if (ft_is_palindrome  "") then print_endline("true")
  else print_endline("false");
    if (ft_is_palindrome  "lamasamal") then print_endline("true")
  else print_endline("false");