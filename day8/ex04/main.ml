(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/01 00:37:15 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 15:28:50 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let string_of_set s =
  let rec aux s =
    match s with
    | head :: [] -> Printf.printf "%s]" (string_of_int head)
    | head :: tail ->
        Printf.printf "%s," (string_of_int head);
        aux tail
    | [] -> Printf.printf "]"
  in
  Printf.printf "[";
  aux s

let () =
  let s1 =
    Set.Set.return 2
    |> Set.Set.union (Set.Set.return 3)
    |> Set.Set.union (Set.Set.return 4)
    |> Set.Set.union (Set.Set.return 42)
  in

  let s2 =
    Set.Set.return 5
    |> Set.Set.union (Set.Set.return 10)
    |> Set.Set.union (Set.Set.return 42)
  in

  Printf.printf "s1: Set.return: ";
  string_of_set s1;
  print_newline ();

  Printf.printf "s2: Set.return: ";
  string_of_set s2;
  print_newline ();

  Printf.printf "Set.bind s1 f a -> a * 2: ";
  string_of_set (Set.Set.bind s1 (fun a -> Set.Set.return (a * 2)));
  print_newline ();

  Printf.printf "Set.bind with f generate duplicate value: ";
  string_of_set
    (Set.Set.bind s2 (fun a ->
         Set.Set.return (if a mod 2 <> 0 then a * 2 else a)));
  print_newline ();

  Printf.printf "Set.union s1 s2: ";
  string_of_set (Set.Set.union s1 s2);
  print_newline ();

  Printf.printf "Set.inter s1 s2: ";
  string_of_set (Set.Set.inter s1 s2);
  print_newline ();

  Printf.printf "Set.diff s1 s2: ";
  string_of_set (Set.Set.diff s1 s2);
  print_newline ();

  Printf.printf "Set.filter s1 f: ";
  string_of_set
    (Set.Set.filter s1 (fun a -> if a mod 2 = 0 then true else false));
  print_newline ();

  Printf.printf "Set.filter s1 f always false ";
  string_of_set (Set.Set.filter s1 (fun _ -> false));
  print_newline ();

  Printf.printf "Set.foreach s1 print_int: ";
  Set.Set.foreach s1 print_int;
  print_newline ();

  Printf.printf "Set.forall s1 f: ";
  if Set.Set.for_all s1 (fun a -> if a > 0 then true else false) then
    print_endline "true"
  else print_endline "false";

  Printf.printf "Set.forall s1 f: ";
  if Set.Set.for_all s1 (fun a -> if a > 2 then true else false) then
    print_endline "true"
  else print_endline "false";

  Printf.printf "Set.forall [] f: ";
  if Set.Set.for_all [] (fun a -> if a > 2 then true else false) then
    print_endline "true"
  else print_endline "false";

  Printf.printf "Set.exists [] f: ";
  if Set.Set.exists [] (fun a -> if a = 2 then true else false) then
    print_endline "true"
  else print_endline "false"
