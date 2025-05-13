(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 18:45:10 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:44:07 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str =
  let length = String.length str in
  if length = 0 then true
  else
    let rec ft_compare_char x last_char =
      if x > last_char / 2 then true
      else if String.get str x = String.get str (last_char - x) then
        ft_compare_char (x + 1) last_char
      else false
    in
    ft_compare_char 0 (length - 1)

let () =
  if ft_is_palindrome "coucou" then print_endline "true"
  else print_endline "false";
  if ft_is_palindrome "maram" then print_endline "true"
  else print_endline "false";
  if ft_is_palindrome "wesh" then print_endline "true"
  else print_endline "false";
  if ft_is_palindrome "" then print_endline "true" else print_endline "false";
  if ft_is_palindrome "lamasamal" then print_endline "true"
  else print_endline "false";
  if ft_is_palindrome "abba" then print_endline "true"
  else print_endline "false"
