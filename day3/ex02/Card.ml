(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/04 11:53:51 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:41:07 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Color = struct
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
end

module Value = struct
  type t =
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | T8
    | T9
    | T10
    | Jack
    | Queen
    | King
    | As

  let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

  let toInt v =
    match v with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13

  let toString v =
    match v with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose v =
    match v with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"

  let next v =
    match v with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "next: As has no next value"

  let previous v =
    match v with
    | T2 -> invalid_arg "previous: Previous has no previous value"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King
end

type t = { color : Color.t; value : Value.t }

let newCard v c = { value = v; color = c }
let allSpades = List.map (fun v -> newCard v Color.Spade) Value.all
let allHearts = List.map (fun v -> newCard v Color.Heart) Value.all
let allDiamonds = List.map (fun v -> newCard v Color.Diamond) Value.all
let allClubs = List.map (fun v -> newCard v Color.Club) Value.all
let all = allSpades @ allHearts @ allDiamonds @ allClubs
let getValue card = card.value
let getColor card = card.color

let toString card =
  Value.toString (getValue card) ^ Color.toString (getColor card)

let toStringVerbose card =
  "Card" ^ "("
  ^ Value.toStringVerbose card.value
  ^ ", "
  ^ Color.toStringVerbose card.color
  ^ ")"

let compare (c1 : t) (c2 : t) =
  if c1.value = c2.value then 0 else if c1.value > c2.value then 1 else -1

let max (c1 : t) (c2 : t) =
  if c1.value = c2.value then c1 else if c1.value > c2.value then c1 else c2

let min (c1 : t) (c2 : t) =
  if c1.value = c2.value then c1 else if c1.value < c2.value then c1 else c2

let best (l : t list) =
  match l with
  | [] -> invalid_arg "List can't be empty"
  | head :: tail -> List.fold_left (fun acc x -> max acc x) head tail

let isOf t c = t.color = c
let isSpade t = t.color = Spade
let isHeart t = t.color = Heart
let isDiamond t = t.color = Diamond
let isClub t = t.color = Club
