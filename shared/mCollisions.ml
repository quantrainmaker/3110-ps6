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
  (* Tests all ufos with all bullets for collisions - Returns ufos
   * with proper health *)
  val ufo_test : ufo*int list -> bullet list -> ufo*int list
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
    List.fold_left (fun acc x -> if power_test pc x then (acc+1) else acc) 0 pl
  let unhit_powers pl pc =
    List.fold_left (fun acc x -> if power_test pc x then acc else (x::acc)) [] pl
  let ufo_test x y = failwith "todo"
end