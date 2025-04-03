(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/03 18:01:24 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/03 18:31:43 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () = 
  let dr = new Doctor.doctor "Octopus" 80 (new People.people "Victoume") in 
  let dalek = new Dalek.dalek in
  let bob = new People.people "Bob" in
  dalek#exterminate(dr#get_sidekick);
  print_endline dalek#talk;
  
  

  
