(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/08 10:43:14 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/19 15:24:49 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let convert_gray n = 
  n lxor (n lsr 1)

let rec print_bits n zero = 
  let rec aux x i = 
    if i = zero then ()
    else begin 
      aux (x lsr 1) (i + 1);
      if ((x land 1) = 1) then print_char '1' 
      else  print_char '0'; end in 
  aux n 0

let gray n = 
  if n < 0 then print_string "Error\n"
  else if n = 0 then print_char '\n'
  else  
    let total = 1 lsl n in  
    let rec aux i =
      if (i < total) then begin
        print_bits (convert_gray i) n; print_char ' ';
        aux (i + 1) end in
    aux 0;
  print_char '\n'

let () = 
  gray (-1);
  gray (0);
  gray 1;
  gray 2;
  gray 2;
  gray 3;
  gray 4;
  gray 5;
  gray 6;