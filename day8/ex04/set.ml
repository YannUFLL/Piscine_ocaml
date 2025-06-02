(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   set.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/31 21:33:50 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/02 10:51:37 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Set = struct
  type 'a t = 'a List.t

  let return x = [ x ]

  let rec bind x f =
    let rec aux = function head :: tail -> f head @ aux tail | [] -> [] in
    let rec remove_duplicate l =
      match l with
      | head :: tail ->
          head
          :: remove_duplicate
               (List.filter (fun x -> if x = head then false else true) tail)
      | [] -> []
    in
    remove_duplicate (aux x)

  let union s1 s2 =
    let rec remove_duplicate l =
      match l with
      | head :: tail ->
          head
          :: remove_duplicate
               (List.filter (fun x -> if x = head then false else true) tail)
      | [] -> []
    in
    remove_duplicate (s1 @ s2)

  let rec inter s1 s2 =
    match s1 with
    | head :: tail ->
        if List.mem head s2 then head :: inter tail s2 else inter tail s2
    | [] -> []

  let rec diff s1 s2 =
    match s1 with
    | head :: tail ->
        if List.mem head s2 then diff tail s2 else head :: diff tail s2
    | [] -> []

  let filter s1 predicate = List.filter predicate s1
  let foreach s f = List.iter f s
  let for_all s predicate = List.for_all predicate s
  let exists s predicate = List.exists predicate s
end
