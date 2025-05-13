(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/28 20:14:47 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:39:34 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig
  val pair : int * int
end

module type VAL = sig
  val x : int
end

module type MAKEPROJECTION = functor (P : PAIR) -> VAL

module MakeFst : MAKEPROJECTION =
functor
  (P : PAIR)
  ->
  struct
    let x = fst P.pair
  end

module MakeSnd : MAKEPROJECTION =
functor
  (P : PAIR)
  ->
  struct
    let x = snd P.pair
  end

module Pair : PAIR = struct
  let pair = (21, 42)
end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
