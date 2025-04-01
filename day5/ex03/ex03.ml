(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/01/29 11:11:08 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/01 14:25:37 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FRACTIONAL_BITS = sig 
  val bits : int
end 

module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int: int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool(* sq *)
  val eqp : t -> t -> bool (** physical equality*)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t 
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
  end

  module Make (F : FRACTIONAL_BITS) : FIXED = struct 
    (* Conteneur *)
    type t = int
    
    (* Helper *)
    let scale = 1 lsl F.bits
    let scale_float = float_of_int scale

    (* Implementation *)
    let of_float (f : float) : int = int_of_float (f *. scale_float)
    let of_int (n : int) : int = n lsl F.bits
    let to_float (x : t) : float = (float_of_int x /. scale_float)
    let to_int (n : t) : int = n lsr F.bits
    let to_string x = string_of_float (to_float x)

    let zero = 0
    let one = scale

    let succ x = x + scale
    let pred x = x - scale
    let min x y = if x < y then x else y
    let max x y = if x > y then x else y 
    let gth x y = x > y
    let lth x y = x < y
    let gte x y = x >= y
    let lte x y = x <= y
    let eqp x y = x == y
    let eqs x y = x = y
    let add x y = x + y
    let sub x y = x - y
    let mul x y = (x * y) / scale
    let div x y = (x * scale) / y
    let foreach start stop f = 
      let rec loop current = 
        if (current <= stop)
        then begin 
          f (current); 
          loop (current + 1) 
        end
        else () 
      in 
      loop start
  end

  module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let module F = Fixed8 in
  let open F in

  let print name x = Printf.printf "%s: %s\n" name (to_string x) in
  let print_bool name b = Printf.printf "%s: %b\n" name b in

  let a = of_float 2.5 in
  let b = of_int 3 in

  print "a" a;
  print "b" b;
  print "of_int 3" (of_int 3);
  print "of_float 2.5" (of_float 2.5);
  print "to_int a" (of_int (to_int a)); (* round trip *)
  print "to_float a" (of_float (to_float a)); (* round trip *)

  print "zero" zero;
  print "one" one;
  print "succ a" (succ a);
  print "pred b" (pred b);

  print "min a b" (min a b);
  print "max a b" (max a b);

  print_bool "gth a b" (gth a b);
  print_bool "lth a b" (lth a b);
  print_bool "gte a b" (gte a b);
  print_bool "lte a b" (lte a b);

  print_bool "eqp a a" (eqp a a);
  print_bool "eqp a b" (eqp a b);
  print_bool "eqs a (of_float 2.5)" (eqs a (of_float 2.5));

  print "add a b" (add a b);
  print "sub b a" (sub b a);
  print "mul a b" (mul a b);
  print "div b a" (div b a);

  Printf.printf "\nforeach from 0 to 1:\n";
  foreach zero one (fun f -> print_endline (to_string f));
