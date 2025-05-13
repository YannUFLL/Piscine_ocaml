(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/08 14:07:40 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:44:15 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n n str =
  let rotate_char n c =
    if c >= 'a' && c <= 'z' then
      if int_of_char c + n > int_of_char 'z' then
        char_of_int
          (int_of_char 'a' + (n - (int_of_char 'z' - int_of_char c + 1)))
      else char_of_int (int_of_char c + n)
    else if c >= 'A' && c <= 'Z' then
      if int_of_char c + n > int_of_char 'Z' then
        char_of_int
          (int_of_char 'A' + (n - (int_of_char 'Z' - int_of_char c + 1)))
      else char_of_int (int_of_char c + n)
    else c
  in
  String.map (rotate_char (n mod 26)) str

let () =
  print_endline (ft_rot_n 8 "abcdefghijklmnopqrstuwxyz");
  print_endline (ft_rot_n 8 "ABCZ");
  print_endline (ft_rot_n 0 "ABC45454Z");
  print_endline (ft_rot_n 58 "ABC45454Z")
