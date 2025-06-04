(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/25 20:36:16 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 13:57:18 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type MONOID = sig
  type element

  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module INT = struct
  type element = int

  let zero1 = 0
  let zero2 = 1
  let add x y = x + y
  let sub x y = x - y
  let mul x y = x * y
  let div x y = x / y
end

module FLOAT = struct
  type element = float

  let zero1 = 0.0
  let zero2 = 1.0
  let add x y = x +. y
  let sub x y = x -. y
  let mul x y = x *. y
  let div x y = x /. y
end

module type CALC = functor (M : MONOID) -> sig
  val add : M.element -> M.element -> M.element
  val sub : M.element -> M.element -> M.element
  val mul : M.element -> M.element -> M.element
  val div : M.element -> M.element -> M.element
  val power : M.element -> int -> M.element
  val fact : M.element -> M.element
end

module Calc : CALC =
functor
  (M : MONOID)
  ->
  struct
    let add x y = M.add x y
    let sub x y = M.sub x y
    let mul x y = M.mul x y
    let div x y = M.div x y

    let rec power e p =
      match p with 0 -> M.zero2 | 1 -> e | _ -> M.mul (power e (p - 1)) e

    let rec fact e =
      if e <= M.zero1 then M.zero2 else M.mul (fact (M.sub e (M.div e e))) e
  end
