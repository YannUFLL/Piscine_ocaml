(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/16 12:55:49 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 13:38:34 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_proj (p, status, g) =
  Printf.printf "Project: %s\nStatus: %s\n Grade: %d\n\n" p status g

let () =
  let p1 = ("Construct the state of the art Tank", "fail", 40) in
  let p2 = ("Be the most handsome man in the world", "succeed", 80) in

  Printf.printf "=== p1 projet ===\n\n";
  print_proj p1;
  Printf.printf "=== p2 projet ===\n\n";
  print_proj p2;

  Printf.printf "=== test ===\n\n";

  Printf.printf
    "1. Must failed beause the avg of the two project are bellow 80:\n";
  print_proj (App.App.combine p1 p2);

  Printf.printf "2. App.success p1:\n";
  print_proj (App.App.success p1);

  Printf.printf "3. App.fail p2:\n";
  print_proj (App.App.fail p2);

  Printf.printf "4. App.combine p1 App.zero:\n";
  print_proj (App.App.combine p1 App.App.zero);

  Printf.printf "5. App.combine p2 App.zero:\n";
  print_proj (App.App.combine p2 App.App.zero);

  Printf.printf "6. App.combine p2 new_project:\n";
  print_proj (App.App.combine p2 ("Best project of the world", "succeed", 180))
