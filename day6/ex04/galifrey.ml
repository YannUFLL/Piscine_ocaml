(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   galifrey.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/04 14:22:21 by ydumaine          #+#    #+#             *)
(*   Updated: 2025/04/04 15:43:08 by ydumaine         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class galifrey =
  object (self)
    val mutable _daleks : Dalek.dalek list = []
    val mutable _doctors : Doctor.doctor list = []
    val mutable _people : People.people list = []

    initializer
      let rec fill_daleks i =
        if i > 0 then begin
          _daleks <- (new Dalek.dalek (if Random.int 2 = 1 then true else false)) :: _daleks;
          fill_daleks (i - 1)
        end
      in
      fill_daleks 10;

      _doctors <- [
        new Doctor.doctor "Benedict Cumberbatch" 25 (new People.people "Tom Holland");
        new Doctor.doctor "Benedict Wong" 50 (new People.people "Titeuf");
        new Doctor.doctor "Mahatma Gandhi" 120 (new People.people "Kasturba Gandhi");
        new Doctor.doctor "Gregory House" 45 (new People.people "Wilson");
        new Doctor.doctor "Docteur Who" 903 (new People.people "Clara Oswald");
        new Doctor.doctor "Dr. Emmett Brown" 65 (new People.people "Marty McFly");
        new Doctor.doctor "Dr. Dolittle" 40 (new People.people "Rex le chien");
        new Doctor.doctor "Dr. Strange" 38 (new People.people "Wong");
        new Doctor.doctor "Dr. Octopus" 50 (new People.people "Peter Parker");
        new Doctor.doctor "Dr. Freud" 82 (new People.people "Carl Jung");
        ];

      _people <- [
        new People.people "Patrick Sébastien";
        new People.people "Chuck Norris";
        new People.people "Marge Simpson";
        new People.people "Zizou";
        new People.people "Pikachu"
      ]


    method private dalek_turn () =
      List.iter
        (fun dalek ->
            match _people with
            | p :: ps -> (dalek#exterminate p; _people <- ps)
            | [] ->
              match _doctors with
              | d :: ds -> d#take_damage (Random.int 40); if d#is_still_alive then () else _doctors <- ds
              | [] -> ())
        _daleks

    method private doctor_turn () =
      List.iter
        (fun doctor ->
            match _daleks with
            | d :: ds -> doctor#attack d (Random.int 30); if d#is_still_alive then () else _daleks <- ds
            | [] -> ())
        _doctors

    method private can_take_nap () = 
      List.iter 
      (fun doctor -> (if Random.int 2 = 1 then doctor#nap else ())) _doctors

    method do_time_war =
      print_endline "🔥 TIME WAR BEGINS 🔥";
      let rec loop round =
        print_endline ("-- Round " ^ string_of_int round ^ " --");
        self#dalek_turn ();
        self#doctor_turn ();
        self#can_take_nap ();
        if _daleks <> [] && _doctors <> [] then loop (round + 1)
        else begin
          print_endline "🏁 TIME WAR ENDED";
          print_endline "\n--- 👥 SURVIVORS ---";
          List.iter (fun d -> print_endline ("Dalek: " ^ d#to_string)) _daleks;
          List.iter (fun d -> print_endline ("Doctor: " ^ d#to_string)) _doctors;
          List.iter (fun p -> print_endline ("People: " ^ p#to_string)) _people
        end
    in
      loop 1
            
  end