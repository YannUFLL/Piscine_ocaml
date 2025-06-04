(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   app.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/16 12:37:11 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/06/04 13:41:04 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type App = sig
  type project = string * string * int

  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

module App : App = struct
  type project = string * string * int

  let zero = ("", "", 0)

  let combine (str1, _, g1) (str2, _, g2) =
    let new_g = (g1 + g2) / 2 in
    (str1 ^ str2, (if new_g > 80 then "succeed" else "fail"), new_g)

  let fail (str1, _, _) = (str1, "fail", 0)
  let success (str1, _, _) = (str1, "succeed", 80)
end
