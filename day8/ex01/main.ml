(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/16 12:55:49 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/16 13:26:59 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_proj (p, status, g) =
  Printf.printf "Project: %s\nStatus: %s\n Grade: %d\n" p status g

let () =
  let p1 = ("Construct the world of tanks", "fail", 40) in
  let p2 = ("Be the most handsome man in the world", "success", 80) in

  print_proj (App.AppMonoid.combine p1 p2)
