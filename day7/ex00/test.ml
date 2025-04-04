(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/04 16:42:07 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/04 17:06:17 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class hat a =
object 
  val _diameter = a
  method get_diameter = _diameter
end 



let () = 
let hat1 = new hat 42 in 
let hat2 = new hat "coucou" in
let make_hat = new hat in
print_string "lol"