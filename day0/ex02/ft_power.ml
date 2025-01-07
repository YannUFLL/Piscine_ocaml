(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/06 16:00:07 by root              #+#    #+#             *)
(*   Updated: 2025/01/07 17:52:22 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_power x y =
  if (y = 0) then (print_int(1); print_char '\n')
  else if (x = 0) then (print_int(0); print_char '\n')
  else 
    let rec power x y = 
      if (y != 0) then (x *(power x (y - 1))) 
      else 1 
    in 
    print_int (power x y);
    print_char '\n'

let () = 
  ft_power 0 1;
  ft_power 1 2;
  ft_power 3 3; 
  ft_power 2 4;

