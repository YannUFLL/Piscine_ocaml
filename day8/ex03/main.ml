(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/31 18:39:34 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 14:35:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let safe_div x y =
  if y = 0 then Try.Try.Failure (Failure "Can't divide by 0")
  else Try.Try.return (x / y)

let () =
  let a = Try.Try.return 10 in
  let b = Try.Try.Failure (Failure "Initial failure") in

  Printf.printf "Return test: %s\n"
    (match Try.Try.return 42 with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Bind test: %s\n"
    (match Try.Try.bind a (fun a -> safe_div a 42) with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Bind test: %s\n"
    (match Try.Try.bind a (fun a -> safe_div a 0) with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Recover test: %s\n"
    (match
       Try.Try.recover b (fun err ->
           match err with
           | Failure msg ->
               if msg = "Initial failure" then Try.Try.return 42
               else Try.Try.return 0
           | _ -> Try.Try.return 84)
     with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Recover test: %s\n"
    (match
       Try.Try.recover a (fun err ->
           match err with
           | Failure msg ->
               if msg = "Initial failure" then Try.Try.return 42
               else Try.Try.return 0
           | _ -> Try.Try.return 84)
     with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Filter test: %s\n"
    (match Try.Try.filter a (fun x -> if x mod 2 = 0 then false else true) with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Filter test: %s\n"
    (match
       Try.Try.filter (Try.Try.return 17) (fun x ->
           if x mod 2 = 0 then false else true)
     with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Flatten test: %s\n"
    (match Try.Try.flatten (Try.Try.return (Try.Try.return 17)) with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  Printf.printf "Flatten test: %s\n"
    (match Try.Try.flatten (Try.Try.return b) with
    | Success x -> string_of_int x
    | Failure _ -> "Fail");

  let result =
    ( (Try.Try.return 100 |> fun m -> Try.Try.bind m (fun x -> safe_div x 2))
    |> fun m -> Try.Try.bind m (fun x -> safe_div x 0) )
    |> fun m -> Try.Try.bind m (fun x -> safe_div x 18)
  in
  Printf.printf "Bind chain result: %s\n"
    (match result with
    | Success x -> string_of_int x
    | Failure exp -> Printexc.to_string exp)
