(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/03 21:01:36 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:43:06 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
  if n < 0 then -1
  else if n = 0 then 0
  else if n = 1 then 1
  else
    let rec loop n a b i = if i <= n then loop n (a + b) a (i + 1) else a + b in
    loop n 0 1 2

let () =
  print_int (fibonacci (-42));
  print_newline ();
  print_int (fibonacci 0);
  print_newline ();
  print_int (fibonacci 1);
  print_newline ();
  print_int (fibonacci 2);
  print_newline ();
  print_int (fibonacci 3);
  print_newline ();
  print_int (fibonacci 6);
  print_newline ();
  print_int (fibonacci 10);
  print_newline ();
  print_int (fibonacci 24);
  print_newline ()
