(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/06 16:00:07 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/01/29 18:34:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_power x y =
  if (y = 0) then 1
  else if (x = 0) then 0
  else 
    let rec power x y = 
      if (y != 0) then (x *(power x (y - 1))) 
      else 1 
    in 
  power x y

let () = 
  print_int (ft_power 0 1);
  print_char ('\n');
  print_int (ft_power 1 2);
  print_char ('\n');
  print_int (ft_power 3 3); 
  print_char ('\n');
  print_int (ft_power 2 4);
  print_char ('\n');