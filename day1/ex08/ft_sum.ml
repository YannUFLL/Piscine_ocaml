(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 22:07:09 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/05 12:04:35 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f lower upper = 
  if upper < lower then nan
  else 
  let rec aux acc n = 
  if n > upper then acc
  else  aux (acc +. f n) (n + 1) in
  aux 0.0 lower
  

  let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10); print_newline ();