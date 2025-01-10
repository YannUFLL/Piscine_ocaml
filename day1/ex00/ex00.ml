(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex00.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/09 13:24:53 by root              #+#    #+#             *)
(*   Updated: 2025/01/09 14:32:07 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringOrder = struct
  type t = string
  let compare = String.compare
end

module StringSet = Set.Make(StringOrder)

let () =
let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
StringSet.iter print_endline set;
print_endline (StringSet.fold ( ^ ) set "");