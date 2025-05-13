(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 15:52:18 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:44:01 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
  let rec create_revers_char_list str x =
    if x >= 0 then String.get str x :: create_revers_char_list str (x - 1)
    else []
  in
  let revers_string = create_revers_char_list str (String.length str - 1) in
  List.iter print_char revers_string;
  print_char '\n'

let () =
  ft_print_rev "Hello World !";
  ft_print_rev ""
