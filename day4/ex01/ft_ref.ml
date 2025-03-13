(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/06 16:24:49 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/13 15:39:43 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable contents : 'a}

let return x = { contents = x }

let get x = x.contents

let set x y = x.contents <- y

let bind (x : 'a ft_ref) (f : 'a -> 'a ft_ref) : 'a ft_ref = f x.contents

let print_result expected result =
  Printf.printf "Result: %s | Expected: %s %s\n"
    result expected (if result = expected then "✅" else "❌")

let () =
  let r = return 42 in
  print_result "42" (string_of_int (get r));

  set r 84;
  print_result "84" (string_of_int (get r));

  set r (-999);
  print_result "-999" (string_of_int (get r));

  let str_ref = return "Hello" in
  print_result "Hello" (get str_ref);

  set str_ref "OCaml";
  print_result "OCaml" (get str_ref);

  let double x = return (x * 2) in
  let r2 = bind r double in
  print_result "-1998" (string_of_int (get r2));

  let greet name = return ("Hello, " ^ name) in
  let r3 = bind str_ref greet in
  print_result "Hello, OCaml" (get r3);

  let float_ref = return 3.14 in
  print_result "3.14" (string_of_float (get float_ref));

  set float_ref 2.718;
  print_result "2.718" (string_of_float (get float_ref));
