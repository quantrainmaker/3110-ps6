open Definitions
open Constants
open Util
open MTeams
open MUfos
open MWeapons

(* Module for handling collision & invincibility functionality *)
module type Collision = sig
  (* Test for player - bullet collision, handles color test *)
  val kevlar_test : player_char -> bullet -> bool
  
  (* Test for ufo - bullet/powerup collisions *)
  val ufo_bullet_test : ufo -> bullet -> bool
  
  (* Decrease a value by one to a minimum of zero *)
  val weaken_shield : int -> int
  
  (* Returns whether player hit by bullet in list *)
  val valid_hit : bullet list -> player_char -> bool
  
  (* Test for player - powerup collision - color not considered *)
  val power_test : player_char -> power -> bool
  
  (* Returns # times player hits power *)
  val power_count : power list -> player_char -> int
  
  (* Returns power list of powers that were not hit *)
  val unhit_powers : power list -> player_char -> power list
  
  (* Updates ufo according to bullet list *)
  val ufo_bullets_update : ufo -> bullet list -> ufo
  
  (* Tests all ufos with all bullets for collisions - Returns ufos
   * with proper hitcount *)
  val ufo_test : (ufo*int) list -> bullet list -> (ufo*int) list
  
  (* Detect if a given bullet collides with any ufos *)
  val dead_bullet : ufo list -> bullet -> bool
  
  (* Return the list of bullets not hit by the ufos *)
  val unhit_bullets : (ufo*int) list -> bullet list -> bullet list
  
  (* Check if the ufo is destroyed or not *)
  val checkhp: ufo -> bool

  (* Return the list of powerups after new ones are scattered *)
  val scatter_powers: (ufo*int) list -> power list -> 
    player_char -> player_char -> power list

  (* Return the list of ufos not destroyed *)
  val delete_ufos: (ufo*int) list -> (ufo*int) list

  (* Returns whether player grazes bullet of opposite color *)
  val graze_check : bullet -> vector -> color -> bool

  (* Returns number of bullets the player has grazed *)
  val graze_count : bullet list -> vector -> color -> int

  (* Returns number of points to award player for grazing *)
  val graze_points : bullet list -> vector -> color -> int

  (* Remove all bullets player grazes *)
  val invin_bullets : bullet list -> vector -> color -> bullet list
end

module Collision_Mechanics : Collision = struct
  let kevlar_test p b = (p.p_color != b.b_color) &&
  (distance p.p_pos b.b_pos <= 
    float_of_int (p.p_radius + b.b_radius))
  
  let ufo_bullet_test u b = distance u.u_pos b.b_pos <= 
    float_of_int (u.u_radius + b.b_radius)
  
  let weaken_shield x = if x > 0 then (x-1) else x
  
  let valid_hit bl pc = 
    List.fold_left (fun acc x -> 
      if kevlar_test pc x then true else acc) false bl
  
  let power_test pc po = (distance pc.p_pos po.b_pos <= 
    float_of_int (pc.p_radius + po.b_radius))
  
  let power_count pl pc = 
    List.fold_left 
      (fun acc x -> if power_test pc x then (acc+1) else acc) 0 pl
  
  let unhit_powers pl pc =
    List.fold_left 
      (fun acc x -> if power_test pc x then acc else (x::acc)) [] pl
  
  let ufo_bullets_update u bl =
    List.fold_left (fun acc x -> 
      if ufo_bullet_test acc x && acc.u_red_hits + acc.u_blue_hits < cUFO_HITS 
      then
        if x.b_color = Red then
          {acc with u_red_hits = (acc.u_red_hits+1)}
        else {acc with u_blue_hits = (acc.u_blue_hits+1)}
      else acc) u bl
  
  let ufo_test x y = 
    List.fold_left (fun acc a -> 
      ((ufo_bullets_update (fst a) y),snd a)::acc) [] x
  
  let dead_bullet u b = 
    List.fold_left 
      (fun acc x -> if ufo_bullet_test x b then true else acc) false u
  
  let unhit_bullets u b = 
    let ufo_list = List.map (fun a -> fst a) u in
    List.fold_left (fun acc x -> 
      if dead_bullet ufo_list x then acc else (x::acc)) [] b

  let checkhp aufo = aufo.u_red_hits + aufo.u_blue_hits >= cUFO_HITS

  let scatter_powers u p pc1 pc2 =
    let rpos = pc1.p_pos in
    let bpos = pc2.p_pos in
    (* create random position given an initial position and radius *)
    let makepos rad pos =
      let newrad = Random.float(float_of_int rad) in
      let vec = rotate (scale newrad (unit_v pos)) (Random.float(2. *. pi)) in
      add_v pos vec in
    (* create a new powerup *)
    let createpow upos ppos col = {
      b_type = Power;
      b_id = next_available_id ();
      b_pos = makepos cUFO_SCATTER_RADIUS upos;
      b_vel = scale (float_of_int cPOWER_SPEED) (unit_v (subt_v ppos upos));
      b_accel = (0., 0.);
      b_radius = cPOWER_RADIUS;
      b_color = col
    } in
    (* create total powerups of a certain color *)
    let rec createpows upos ppos col total count acc =
      if count < total
      then createpows upos ppos col total (count+1) 
        ((createpow upos ppos col)::acc)
      else acc in
    (* if the ufo is dead, scatter a proportion of powerups in the
       red char's direction and the rest in the blue char's direction *)
    let scatter acc atup =
      let aufo = fst atup in
      if checkhp aufo
      then 
        let rpow = int_of_float ((float_of_int aufo.u_red_hits) /. 
          (float_of_int cUFO_HITS) *. (float_of_int cUFO_POWER_NUM)) in
        let upos = aufo.u_pos in
        createpows upos bpos Blue (cUFO_POWER_NUM - rpow) 0 
          (createpows upos rpos Red rpow 0 acc) 
      else acc in
    List.fold_left scatter p u

    let delete_ufos u = List.filter (fun atup -> not (checkhp (fst atup))) u

    let graze_check bull pos col =
      (distance pos bull.b_pos <= 
          float_of_int (cGRAZE_RADIUS)) && 
          (distance pos bull.b_pos > 
            float_of_int (cHITBOX_RADIUS + bull.b_radius)) &&
          (not (col = bull.b_color))

    let graze_count b_list pos color =
      List.fold_left (fun acc x -> 
        if graze_check x pos color then
          (acc+1) else acc) 0 b_list

    let graze_points b_list pos color =
      (graze_count b_list pos color) * cGRAZE_POINTS

    let invin_bullets b_list pos color =
      List.fold_left (fun acc x -> 
        if graze_check x pos color then acc else x::acc) [] b_list
end