(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/15 18:37:25 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/15 18:51:05 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type Watchtower = sig
  type hour = int

  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower : Watchtower = struct
  type hour = int

  let zero = 0
  let add a b = (a + b) mod 12
  let sub a b = (a - b + 12) mod 12
end
