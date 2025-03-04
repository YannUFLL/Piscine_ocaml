(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/03 19:10:03 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/04 16:22:52 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
print_newline ();
print_string "Testing Value.toString test with Value.all: ";print_newline ();
let display_value value = print_string (Value.toString value); print_char ' ' in 
List.iter display_value Value.all;  print_newline (); print_newline ();

print_string "Testing Value.toInt test with Value.all: ";print_newline ();
let display_value value = print_int (Value.toInt value); print_char ' ' in 
List.iter display_value Value.all;  print_newline (); print_newline ();

print_string "Testing Value.toStringVerbose test with Value.all: ";print_newline ();
let display_value_verbose value = print_string (Value.toStringVerbose value); print_char ' ' in
List.iter display_value_verbose Value.all; 
print_newline (); print_newline ();

print_string "Testing Value.next with t5: ";print_newline ();
let t5 = Value.T5 in
print_string (Value.toString t5); print_char ' ';
print_string (Value.toString (Value.next t5)); print_newline ();print_newline ();

print_string "Testing Value.previous with t5: "; print_newline ();
let t5 = Value.T5 in
print_string (Value.toString t5); print_string " ";
print_string (Value.toString (Value.previous t5)); print_newline ();print_newline ();

print_string "Testing Value.next with as (Should throw an exception): "; print_newline ();
let a = Value.As in
try
print_string (Value.toString a); print_string " ";
print_string (Value.toString (Value.next a));
with
| Invalid_argument msg -> print_endline ("Caught exception: " ^ msg);
print_newline ();print_newline ();

print_string "Testing Value.previous with t2 (Should throw an exception): "; print_newline ();
let t2   = Value.T2 in
try
  print_string (Value.toString t2); print_string " ";
  print_string (Value.toString (Value.previous t2));
with
| Invalid_argument msg -> print_endline ("Caught exception: " ^ msg);
print_newline ();print_newline ();
