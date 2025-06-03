(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 15:34:40 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/03 15:02:21 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class test_reaction =
  object (self)
    inherit Reaction.reaction [] []
    method get_result = []
    method get_start = []
    method balance = (self :> Reaction.reaction)
    method is_balanced = true
  end

let () =
  let r = new test_reaction in
  let start = r#get_start in
  let result = r#get_result in
  if start = result then print_endline "true" else print_endline "false";
  let balanced = r#is_balanced in
  if balanced then print_endline "true" else print_endline "false"
