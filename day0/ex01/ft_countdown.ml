(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 17:34:30 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/01/31 10:22:47 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x = 
  if (x < 0) then 
    begin 
      print_int 0;
      print_char '\n'
    end 
  else 
    begin
      print_int x;
      print_char '\n';
      if (x > 0) then ft_countdown(x - 1)
    end

let () = 
  ft_countdown(9);
  ft_countdown(0);
  ft_countdown(4);
  ft_countdown(-1);