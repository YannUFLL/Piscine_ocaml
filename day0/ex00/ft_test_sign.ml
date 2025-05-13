(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 17:31:10 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:43:46 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign x =
  if x >= 0 then print_endline "positive" else print_endline "negative"

let () =
  ft_test_sign 42;
  ft_test_sign (-42);
  ft_test_sign 0
