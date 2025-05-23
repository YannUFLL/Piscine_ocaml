(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/06 15:57:15 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:39:54 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let my_sleep () = Unix.sleep 1

let () =
  try
    let max = int_of_string Sys.argv.(1) in
    let i = ref 0 in
    while !i < max do
      my_sleep ();
      i := !i + 1
    done
  with _ -> ()
