open Definitions
open Constants
open Util

(* Module to handle UFO functionality *)
module type UFO = sig
  (* Find random point for UFO to go to *)
  val targeted : unit -> vector
  (* Create random velocity vector base on curr. pos. with UFO speed *)
  val alien_path : vector -> vector
  (* Default UFO *)
  val base_ufo : unit -> ufo
  (* Change UFO direction to random point on screen *)
  val redirect : ufo -> ufo
  (* UFO takes a hit *)
  val suffer_hit : ufo -> color -> ufo
  (* Check if UFO destroyed *)
  val destroyed : ufo -> bool
  (* Updates the Ufo's position *)
  val move_ufo : ufo -> ufo
  (* Updates positions (and velocities when necessary) of Ufos
   * The tuple int tracks the time the Ufo was created 
   * The second int is the current time *)
  val batch_ufo : (ufo*int) list -> int -> (ufo*int) list
end

(* UFO functions *)
module UFO_Mechanics : UFO = struct
  let targeted () = 
    let vex = Random.float (float_of_int cBOARD_WIDTH) in
    let vey = Random.float (float_of_int cBOARD_HEIGHT) in
    (vex,vey)
  let alien_path position = 
    let base_vector = unit_v (subt_v (targeted ()) position) in
    scale (float_of_int cUFO_SPEED) base_vector
  let base_ufo x = 
    let rex = Random.float 0.25 +. (Random.float 0.50) in
    let rey = float_of_int (Random.int 2) in
    let pos = (rex *. float_of_int (cBOARD_WIDTH), 
    	rey *. float_of_int (cBOARD_HEIGHT)) in 
    {u_id = (next_available_id ()); u_pos = pos; u_vel = (alien_path pos); 
      u_radius = cUFO_RADIUS; u_red_hits = 0; u_blue_hits = 0}
  let redirect x = {x with u_vel = (alien_path x.u_pos)}
  let suffer_hit x col = 
    if col = Red then
      {x with u_red_hits = (x.u_red_hits + 1)}
    else
      {x with u_blue_hits = (x.u_blue_hits + 1)}
  let destroyed x = 
    if x.u_red_hits + x.u_blue_hits >= cUFO_HITS then true else false
  let move_ufo x = 
    let new_pos = add_v x.u_pos x.u_vel in
    {x with u_pos = new_pos}
  let batch_ufo l i =
    List.fold_left (fun acc x -> 
      if (i - (snd x)) mod cUFO_MOVE_INTERVAL = 0 && (snd x) < i then
        let j = move_ufo (redirect (fst x)) in
        (j,(snd x)+1)::acc
      else
        let k = move_ufo (fst x) in
        (k,(snd x)+1)::acc) [] l
end