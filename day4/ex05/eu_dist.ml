(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/10 16:50:25 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/05/13 18:40:16 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let eu_dist a b =
  if Array.length a <= 0 then
    raise (Invalid_argument "Array length must be > 0")
  else
    let acc = ref 0.0 in
    for i = 0 to Array.length a - 1 do
      acc := !acc +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
    done;
    sqrt !acc

let print_result expected result =
  Printf.printf "Result: %.3f | Expected: %.3f %s\n" result expected
    (if abs_float (result -. expected) < 1e-3 then "✅" else "❌")

let () =
  (* Basic tests *)
  print_result 2.0 (eu_dist [| 1.0 |] [| -1.0 |]);
  print_result 0.0 (eu_dist [| 0.0; 0.0 |] [| 0.0; 0.0 |]);
  print_result 5.0 (eu_dist [| 1.0; 2.0 |] [| 4.0; 6.0 |]);

  (* Larger vectors *)
  print_result 7.810 (eu_dist [| 1.0; 2.0; 3.0 |] [| 4.0; 6.0; 9.0 |]);
  print_result 0.0
    (eu_dist [| 10.0; 20.0; 30.0; 40.0 |] [| 10.0; 20.0; 30.0; 40.0 |]);

  (* Edge case: Large distance *)
  print_result 1414213562.373 (eu_dist [| 1.0e9; 1.0e9 |] [| 0.0; 0.0 |])
