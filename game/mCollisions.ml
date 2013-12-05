open Definitions
open Constants
open Util
open MTeams
open MUfos
open MWeapons
open Netgraphics

(* Module for handling collision & invincibility functionality *)
module type Collision = sig
  (* Test for ufo - bullet/powerup collisions *)
  val ufo_bullet_test : ufo -> bullet -> bool
  
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

  (* Returns number of points to award player for grazing *)
  val graze_points : bullet list -> vector -> color -> int

  (* Remove all bullets player grazes *)
  val invin_bullets : bullet list -> vector -> color -> bullet list
end

module Collision_Mechanics : Collision = struct
  (**************** Helper Functions (hidden by interface) *******************)
  (* Test for player - bullet collision, handles color test *)
  let kevlar_test p b = (p.p_color <> b.b_color) &&
    (distance p.p_pos b.b_pos <= float_of_int (p.p_radius + b.b_radius))
  (* Returns whether player grazes bullet of opposite color *)
  let graze_check bull pos col =
    let dist = distance pos bull.b_pos in
    (dist <= float_of_int (cGRAZE_RADIUS)) && 
    (dist > float_of_int (cHITBOX_RADIUS + bull.b_radius)) &&
    (col <> bull.b_color)
  (* Returns number of bullets the player has grazed *) 
  let graze_count b_list pos color =
    List.fold_left (fun acc bul -> 
      if graze_check bul pos color 
      then 
        let _ = add_update Graze in
        acc+1 
      else acc) 0 b_list
  

  (****************** Implemented Module Functions **********************)
  let ufo_bullet_test u b = distance u.u_pos b.b_pos <= 
    float_of_int (u.u_radius + b.b_radius)
  
  let valid_hit bl pc = 
    List.fold_left (fun acc bul -> 
      if kevlar_test pc bul then true else acc) false bl
  
  let power_test pc po = (distance pc.p_pos po.b_pos <= 
    float_of_int (pc.p_radius + po.b_radius))
  
  let power_count pl pc = 
    List.fold_left 
      (fun acc pow -> if power_test pc pow then (acc+1) else acc) 0 pl
  
  let unhit_powers pl pc =
    List.fold_left (fun acc pow -> 
      if power_test pc pow 
      then 
        let _ = add_update(DeleteBullet (pow.b_id)) in
        acc 
      else (pow::acc)) [] pl
  
  let ufo_bullets_update u bl =
    List.fold_left (fun auf b -> 
      if ufo_bullet_test auf b && auf.u_red_hits + auf.u_blue_hits < cUFO_HITS
      then
        if b.b_color = Red 
        then {auf with u_red_hits = (auf.u_red_hits+1)}
        else {auf with u_blue_hits = (auf.u_blue_hits+1)}
      else auf) u bl
  
  let ufo_test u b = 
    List.fold_left (fun acc tup -> 
      ((ufo_bullets_update (fst tup) b),snd tup)::acc) [] u
  
  let dead_bullet u b = 
    List.fold_left 
      (fun acc tup -> if ufo_bullet_test tup b then true else acc) false u
  
  let unhit_bullets u b = 
    let ufo_list = List.map (fun tup -> fst tup) u in
    List.fold_left (fun acc bul -> 
      if dead_bullet ufo_list bul 
      then 
        let _ = add_update(DeleteBullet (bul.b_id)) in
        acc 
      else (bul::acc)) [] b

  let graze_points b_list pos color =
    (graze_count b_list pos color) * cGRAZE_POINTS

  let invin_bullets b_list pos color =
    List.fold_left (fun acc bul -> 
      if graze_check bul pos color 
      then 
        let _ = add_update(DeleteBullet (bul.b_id)) in
        acc 
      else bul::acc) [] b_list
end