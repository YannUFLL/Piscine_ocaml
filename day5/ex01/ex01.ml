(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/09 14:59:25 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/01 14:45:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHash = struct
    type t = string
    let equal s1 s2 = s1 = s2
    let hash s = 
        let rec hashFunction s i acc = 
            if (i < String.length s) then
                hashFunction s (i + 1) (acc + (Hashtbl.hash (String.get s i)))
            else
                acc
            in 
            hashFunction s 0 0
    end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht