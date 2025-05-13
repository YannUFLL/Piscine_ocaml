(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 15:50:39 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:40:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let jokes =
  [|
    "Pourquoi les bières sont toujours stressées? Parce qu’elles ont la \
     pression";
    "C'est l'histoire d'un papier qui tombe à l'eau. Il crie « Au secours ! \
     J’ai pas pied !";
    "Quel est l’aliment le plus drôle? Bah, le riz (quelle question)";
    "Où vont les biscottes pour danser? En biscothèque";
    "C’est quoi une chauve souris avec une perruque? Une souris.";
  |]

let () =
  Random.self_init ();
  let index = Random.int (Array.length jokes) in
  print_endline jokes.(index)
