(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/06 16:24:49 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/03/07 15:48:09 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable contents : 'a}

let return x = { contents = x }

let get x = x.contents

let set x y = x.contents <- y

let bind (x : 'a ft_ref) (f : 'a -> 'a ft_ref) : 'a ft_ref = f x.contents