(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/03 17:42:33 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:40:48 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

let all = [ Spade; Heart; Diamond; Club ]

let toString card =
  match card with Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"

let toStringVerbose card =
  match card with
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
