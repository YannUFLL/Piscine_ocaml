(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/04 11:41:58 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:37:57 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class ['a] army =
  object
    val mutable _members = ([] : 'a list)
    method add instance = _members <- instance :: _members

    method delete =
      match _members with [] -> () | head :: tail -> _members <- tail

    method get_army_size = List.length _members

    method get_member : 'a =
      match _members with
      | [] -> failwith "empty army"
      | head :: tail ->
          _members <- tail;
          head
  end
