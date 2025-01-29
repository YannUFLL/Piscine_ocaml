(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 17:34:30 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/01/29 18:34:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x = 
  print_int(x);
  print_char('\n');
  if (x > 0) then ft_countdown(x - 1)

let () = 
  ft_countdown(9);
  ft_countdown(0);
  ft_countdown(4);
  
