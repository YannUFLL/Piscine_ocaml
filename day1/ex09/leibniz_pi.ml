(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 22:30:39 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/05 12:11:21 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi delta = 
  if delta < 0.0 then (-1) 
  else 
  let ft_power x y =
    if (y = 0) then 1
    else if (x = 0) then 0
    else 
      let rec power x y = 
        if (y != 0) then (x *(power x (y - 1))) 
        else 1 
      in 
    power x y
    in 
  let rec aux acc i = 
    let diff = 4.0 *. acc -. 4.0 *. atan 1.0 in
    let diff_abs = if diff < 0.0 then -.diff else diff in
    if diff_abs < delta then i
    else aux ( acc +. ((float_of_int (ft_power (-1) i)) /. (2.0 *. (float_of_int i) +. 1.0))) (i + 1) in
    aux ((float_of_int (ft_power (-1) 0)) /. (2.0 *. 0.0 +. 1.0)) 1 

  let () = 
    print_int (leibniz_pi 0.1); print_newline ();
    print_int (leibniz_pi 0.0001); print_newline ();