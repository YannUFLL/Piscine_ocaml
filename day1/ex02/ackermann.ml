(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/03 14:00:19 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/03 17:28:33 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let rec ackermann m n  =
  if (m < 0 || n < 0) then -1 
  else if (m = 0) then n + 1
  else if (m > 0 && n = 0) then ackermann (m - 1) 1
  else  ackermann (m - 1) (ackermann m (n - 1))

let () = 
  print_int (ackermann (-1) 0);
  print_int (ackermann 0 0);
  print_int (ackermann 2 3);
  print_int (ackermann 4 1);