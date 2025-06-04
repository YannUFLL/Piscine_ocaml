(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/15 18:48:06 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 13:12:45 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let h1 = 10 in
  let h2 = 4 in

  let added = Watchtower.Watchtower.add h1 h2 in
  let subbed = Watchtower.Watchtower.sub h1 h2 in

  Printf.printf "%d %d added: %d\n" h1 h2 added;
  Printf.printf "%d %d subbed 12: %d\n" h1 h2 subbed;

  Printf.printf "zero: %d\n" Watchtower.Watchtower.zero;
  Printf.printf "add %d with zero: %d\n" h1
    (Watchtower.Watchtower.add h1 Watchtower.Watchtower.zero);
  Printf.printf "add %d %d: %d\n" 12 12
    (Watchtower.Watchtower.add 12 12)
