(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 21:34:34 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/04 22:04:36 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
  if n <= 0 then false
  else if (x = f x) then true
  else converges f (f x) (n - 1)

let () = 
  converges (fun x -> x / 2) 2 4 
  |> Format.printf "%b\n";
  converges (fun x -> x * x) 1 5 
  |> Format.printf "%b\n";
  converges (( * ) 2) 2 5 
  |> Format.printf "%b\n"