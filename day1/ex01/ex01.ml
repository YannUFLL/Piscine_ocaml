(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/09 14:59:25 by root              #+#    #+#             *)
(*   Updated: 2025/01/09 16:50:45 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module IntHash = struct 
    type t = string
    let equal i j = i=j 


let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht