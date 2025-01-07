(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: root <root@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/07 12:15:16 by root              #+#    #+#             *)
(*   Updated: 2025/01/07 12:46:00 by root             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () = 
    let rec ft_print_ascii x y = 
        if (x <= y) then 
            (print_char (char_of_int(x)); ft_print_ascii (x + 1) y)
    in ft_print_ascii 97 122;
    print_char '\n'


let () = 
    ft_print_alphabet ()