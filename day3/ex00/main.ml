(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/03 18:15:40 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/04 16:23:01 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
print_newline ();
print_string "Testing Color.toString with Color.Club: ";print_newline ();
print_string (Color.toString Color.Club);
print_newline (); print_newline ();

print_string "Testing Color.toString with Color.Heart: ";print_newline ();
print_string (Color.toStringVerbose Color.Heart);
print_newline (); print_newline ();

print_string "Testing Color.toString with Color.all: ";print_newline ();
let display_color color = print_string (Color.toString color); print_newline () in 
List.iter display_color Color.all;  
print_newline ();

print_string "Testing Color.toStringVerbose with Color.all: ";print_newline ();
let display_color_verbose color = print_string (Color.toStringVerbose color); print_newline () in
List.iter display_color_verbose Color.all;
print_newline ();





