(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/31 13:48:29 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 14:45:30 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Try = struct
  type 'a t = Success of 'a | Failure of exn

  let return (x : 'a) = Success x

  let bind (b : 'a t) (f : 'a -> 'b t) =
    match b with
    | Failure err -> Failure err
    | Success x -> ( try f x with e -> Failure e)

  let recover (b : 'a t) (f : exn -> 'a t) =
    match b with Failure err -> f err | Success x -> b

  let filter (a : 'a t) (p : 'a -> bool) =
    match a with
    | Success x ->
        if p x then a else Failure (Failure "Predicate not satisfate")
    | Failure err -> a

  let flatten (a : 'a t t) =
    match a with
    | Failure e_outer -> Failure e_outer
    | Success (Failure e_inner) -> Failure e_inner
    | Success (Success v) -> Success v
end
