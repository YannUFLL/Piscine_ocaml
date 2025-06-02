(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/15 18:48:06 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/16 12:36:33 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let h1 = 10 in
  let h2 = 4 in

  let added = Watchtower.Watchtower.add h1 h2 in
  let subbed = Watchtower.Watchtower.sub h1 h2 in

  Printf.printf "%d + %d mod 12 = %d\n" h1 h2 added;
  Printf.printf "%d - %d mod 12 = %d\n" h1 h2 subbed;

  Printf.printf "add zero %d = %d\n" h1 (Watchtower.Watchtower.add h1 Watchtower.Watchtower.zero);
  Printf.printf "add %d zero = %d\n" h1 (Watchtower.Watchtower.add Watchtower.Watchtower.zero h1)
