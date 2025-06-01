(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/05/25 21:06:10 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/25 21:30:02 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Calc_int = Ex02.Calc(Ex02.INT)
module Calc_float = Ex02.Calc(Ex02.FLOAT)

let () =
  Printf.printf "\n🔢 POWER:\n";
  Printf.printf "- int 3^3     = %d (Expected: 27)\n" (Calc_int.power 3 3);
  Printf.printf "- float 3^3   = %.1f (Expected: 27.0)\n" (Calc_float.power 3.0 3);

  Printf.printf "- int 1^0     = %d (Expected: 1)\n" (Calc_int.power 1 0);
  Printf.printf "- float 1^0   = %.1f (Expected: 1.0)\n" (Calc_float.power 1.0 0);

  Printf.printf "- int 0^1     = %d (Expected: 0)\n" (Calc_int.power 0 1);
  Printf.printf "- float 0^1   = %.1f (Expected: 0.0)\n" (Calc_float.power 0.0 1);

  Printf.printf "- int 7^0     = %d (Expected: 1)\n" (Calc_int.power 7 0);
  Printf.printf "- float 7^0   = %.1f (Expected: 1.0)\n" (Calc_float.power 7.0 0);

  Printf.printf "- int 0^7     = %d (Expected: 0)\n" (Calc_int.power 0 7);
  Printf.printf "- float 0^7   = %.1f (Expected: 0.0)\n" (Calc_float.power 0.0 7);

  Printf.printf "\n➕ ADDITION:\n";
  Printf.printf "- int:   %d (Expected: 42)\n" (Calc_int.add 20 22);
  Printf.printf "- float: %.1f (Expected: 42.0)\n" (Calc_float.add 20.0 22.0);

  Printf.printf "- int:   %d (Expected: 0)\n" (Calc_int.add 0 0);
  Printf.printf "- float: %.1f (Expected: 0.0)\n" (Calc_float.add 0.0 0.0);

  Printf.printf "\n✖️ MULTIPLICATION:\n";
  Printf.printf "- int:   %d (Expected: 84)\n" (Calc_int.mul 21 4);
  Printf.printf "- float: %.1f (Expected: 84.0)\n" (Calc_float.mul 21.0 4.0);

  Printf.printf "- int:   %d (Expected: 0)\n" (Calc_int.mul 0 99);
  Printf.printf "- float: %.1f (Expected: 0.0)\n" (Calc_float.mul 0.0 99.0);

  Printf.printf "\n➖ SUBTRACTION:\n";
  Printf.printf "- int:   %d (Expected: -5)\n" (Calc_int.sub 5 10);
  Printf.printf "- float: %.1f (Expected: -5.0)\n" (Calc_float.sub 5.0 10.0);

  Printf.printf "- int:   %d (Expected: 0)\n" (Calc_int.sub 0 0);
  Printf.printf "- float: %.1f (Expected: 0.0)\n" (Calc_float.sub 0.0 0.0);

  Printf.printf "\n➗ DIVISION:\n";
  Printf.printf "- int:   %d (Expected: 3)\n" (Calc_int.div 9 3);
  Printf.printf "- float: %.1f (Expected: 3.0)\n" (Calc_float.div 9.0 3.0);

  Printf.printf "- int:   %d (Expected: 0)\n" (Calc_int.div 0 9);
  Printf.printf "- float: %.1f (Expected: 0.0)\n" (Calc_float.div 0.0 9.0);

  Printf.printf "\n🧮 FACTORIAL:\n";
  Printf.printf "- int fact 0 = %d (Expected: 1)\n" (Calc_int.fact 0);
  Printf.printf "- float fact 0.0 = %.1f (Expected: 1.0)\n" (Calc_float.fact 0.0);

  Printf.printf "- int fact 1 = %d (Expected: 1)\n" (Calc_int.fact 1);
  Printf.printf "- float fact 1.0 = %.1f (Expected: 1.0)\n" (Calc_float.fact 1.0);

  Printf.printf "- int fact 5 = %d (Expected: 120)\n" (Calc_int.fact 5);
  Printf.printf "- float fact 5.0 = %.1f (Expected: 120.0)\n" (Calc_float.fact 5.0);

  Printf.printf "- int fact 7 = %d (Expected: 5040)\n" (Calc_int.fact 7);
  Printf.printf "- float fact 7.0 = %.1f (Expected: 5040.0)\n" (Calc_float.fact 7.0)
