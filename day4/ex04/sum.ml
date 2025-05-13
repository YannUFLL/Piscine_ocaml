(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/10 16:46:01 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:40:10 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum a b = a +. b

let () =
  print_float (sum 5.0 7.0);
  print_newline ();
  print_float (sum 5.0 7.0);
  print_newline ();
  print_float (sum 0.0 0.0);
  print_newline ();
  print_float (sum (-1.0) (-1.55555));
  print_newline ();
  print_float (sum 5.0 7.0);
  print_newline ();
  print_float (sum 10.1 10.1);
  print_newline ();
  print_float (sum 3.333 3.333);
  print_newline ()
