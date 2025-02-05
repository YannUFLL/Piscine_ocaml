(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 22:07:09 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/04 22:17:11 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_sum f lower upper = 
  if upper < lower then nan
  else if lower == upper then f lower
  else f lower +. ft_sum f (lower + 1) upper

  let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10); print_newline ();