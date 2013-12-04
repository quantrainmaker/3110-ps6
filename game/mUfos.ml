open Definitions
open Constants
open Util
open Netgraphics

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
  (* Return the list of powerups after new ones are scattered *)
  val scatter_powers: (ufo*int) list -> power list -> 
    player_char -> player_char -> power list
  (* Return the list of ufos not destroyed *)
  val delete_ufos: (ufo*int) list -> (ufo*int) list
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
  let base_ufo () = 
    let rex = Random.float 0.25 +. (Random.float 0.50) in
    let rey = float_of_int (Random.int 2) in
    let pos = (rex *. float_of_int (cBOARD_WIDTH), 
    	rey *. float_of_int (cBOARD_HEIGHT)) in 
    let uf = {
      u_id = (next_available_id ()); 
      u_pos = pos; 
      u_vel = (alien_path pos); 
      u_radius = cUFO_RADIUS;
      u_red_hits = 0; 
      u_blue_hits = 0
    } in
    (add_update (AddUFO (uf.u_id, uf.u_pos)));
    uf

  let redirect uf = {uf with u_vel = (alien_path uf.u_pos)}
  let suffer_hit uf col = 
    if col = Red then
      {uf with u_red_hits = (uf.u_red_hits + 1)}
    else
      {uf with u_blue_hits = (uf.u_blue_hits + 1)}
  let destroyed uf = uf.u_red_hits + uf.u_blue_hits >= cUFO_HITS
  let move_ufo uf = 
    let new_pos = add_v uf.u_pos uf.u_vel in
    add_update(MoveUFO (uf.u_id, new_pos));
    {uf with u_pos = new_pos}
  let batch_ufo ulst anint =
    List.fold_left (fun acc tup ->
      let uint = snd tup in
      let uf = fst tup in
      if (anint - uint) mod cUFO_MOVE_INTERVAL = 0 && uint < anint 
      then
        let newuf = move_ufo (redirect uf) in
        (newuf,uint+1)::acc
      else
        let newuf = move_ufo uf in
        (newuf,uint+1)::acc) [] ulst

  let scatter_powers u p pc1 pc2 =
    let rpos = pc1.p_pos in
    let bpos = pc2.p_pos in
    (* create random position given an initial position and radius *)
    let makepos rad pos =
      let newrad = Random.float(float_of_int rad) in
      let vec = rotate (scale newrad (unit_v pos)) (Random.float(2. *. pi)) in
      add_v pos vec in
    (* create a new powerup *)
    let createpow upos ppos col = 
      let pow = {
        b_type = Power;
        b_id = next_available_id ();
        b_pos = makepos cUFO_SCATTER_RADIUS upos;
        b_vel = scale (float_of_int cPOWER_SPEED) (unit_v (subt_v ppos upos));
        b_accel = (0., 0.);
        b_radius = cPOWER_RADIUS;
        b_color = col
      } in
      add_update (AddBullet (pow.b_id, pow.b_color, pow.b_type, pow.b_pos));
      pow in
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
      if destroyed aufo
      then 
        let rpow = int_of_float ((float_of_int aufo.u_red_hits) /. 
          (float_of_int cUFO_HITS) *. (float_of_int cUFO_POWER_NUM)) in
        let upos = aufo.u_pos in
        createpows upos bpos Blue (cUFO_POWER_NUM - rpow) 0 
          (createpows upos rpos Red rpow 0 acc) 
      else acc in
    List.fold_left scatter p u

    let delete_ufos u = List.filter (fun tup -> 
      let uf = fst tup in
      let abool = not (destroyed uf) in
      if abool then abool 
      else 
        let _ = add_update(DeleteUFO (uf.u_id)) in
        abool) u
end