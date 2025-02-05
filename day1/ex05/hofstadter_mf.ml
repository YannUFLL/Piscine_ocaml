(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/04 14:24:28 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/02/05 11:15:42 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n=
  if n = 0 then 1
  else if n > 0 then n - hfs_m(hfs_f (n - 1))
  else -1
and  hfs_m n =
  if n = 0 then 0
  else if n > 0 then n - hfs_f(hfs_m (n - 1))
  else -1

let () =
  print_int (hfs_m 0); print_newline ();
  print_int (hfs_f 0); print_newline ();
  print_int (hfs_m 4); print_newline ();
  print_int (hfs_f 4); print_newline ();