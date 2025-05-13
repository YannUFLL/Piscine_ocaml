(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/08 10:43:14 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:42:07 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let convert_gray n = n lxor (n lsr 1)

let rec print_bits n zero =
  let rec aux x i =
    if i = zero then ()
    else (
      aux (x lsr 1) (i + 1);
      if x land 1 = 1 then print_char '1' else print_char '0')
  in
  aux n 0

let gray n =
  if n < 0 then print_string "Error\n"
  else if n = 0 then print_char '\n'
  else
    let total = 1 lsl n in
    let rec aux i =
      if i < total then (
        print_bits (convert_gray i) n;
        print_char ' ';
        aux (i + 1))
    in
    aux 0;
    print_char '\n'

let () =
  gray (-1);
  print_newline ();
  gray 0;
  print_newline ();
  gray 1;
  print_newline ();
  gray 2;
  print_newline ();
  gray 2;
  print_newline ();
  gray 3;
  print_newline ();
  gray 4;
  print_newline ();
  gray 5;
  print_newline ();
  gray 6
