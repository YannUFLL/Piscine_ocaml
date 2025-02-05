(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 18:18:35 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/05 11:17:05 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec iter f x n =
  if n < 0 then -1
  else if n = 0 then x
  else f (iter f x (n - 1))

let () = 
  iter (fun x -> x * x) 2 4 
  |> Format.printf "%d\n";
  iter (fun x -> x * x) 1 5 
  |> Format.printf "%d\n";
  iter (fun x -> x * 2) 2 4 
  |> Format.printf "%d\n";
  iter (fun x -> x * 2) 2 (-5) 
  |> Format.printf "%d\n"