open Definitions
open Constants
open Util
open MTeams
open Netgraphics

(* Handle weapon functionality - Bullets *)
module type Weapons = sig
  (* Return bullets list - Fire in the hole! *)
  val deploy : color -> action -> position -> bullet list
  (* Update list of bullets, removing those that are off the field 
   * This works for powerups as well *)
  val batch_bullets : bullet list -> bullet list
  (* Remove all bullets due to a hit or bomb *)
  val remove_bullets: bullet list -> bullet list
end

module Weapon_Mechanics : Weapons = struct
  (* create a new bullet given type,position,velocity,acceleration,and color *)
  let createbul typ pos vel acc col =
    let valid_acc = if magnitude acc > cACCEL_LIMIT then (0., 0.) else acc in
    let bul = {
      b_type = typ;
      b_id = next_available_id ();
      b_pos = pos;
      b_vel = vel;
      b_accel = valid_acc;
      b_radius = radius_of_bullet typ;
      b_color = col
    } in
    add_update (AddBullet (bul.b_id, bul.b_color, bul.b_type, bul.b_pos));
    bul
  (* Create list of bullets of Spread type *)  
  let rec spread targ acc col tracker storage position = 
    if int_of_float tracker >= cSPREAD_NUM then storage
    else
      let base_vector = subt_v targ position in
      let scaled_vector = 
        scale (float_of_int cSPREAD_SPEED) (unit_v base_vector) in
      let interval = 2.0 *. pi /. (float_of_int cSPREAD_NUM) in
      let rotated_vel = rotate scaled_vector (tracker *. interval) in
      let nb = createbul Spread position rotated_vel acc col in
      spread targ acc col (tracker +. 1.0) (nb::storage) position
  (* Create bullet of Bubble type *)
  let bubble targ acc col position = 
    let base_vector = subt_v targ position in
    let scaled_vector = 
      scale (float_of_int cBUBBLE_SPEED) (unit_v base_vector) in
    let nb = createbul Bubble position scaled_vector acc col in
    [nb]
  (* Create list of bullets of Trail type *)
  let rec trail targ acc col tracker storage position = 
    if tracker >= cTRAIL_NUM then storage
    else
      let speed_factor = (tracker + 1) * cTRAIL_SPEED_STEP in
      let base_vector = subt_v targ position in
      let scaled_vector = 
        scale (float_of_int speed_factor) (unit_v base_vector) in
      let b_straight = createbul Trail position scaled_vector acc col in 
      let b_left = createbul Trail position 
        (rotate_deg scaled_vector (float_of_int cTRAIL_ANGLE)) acc col in
      let b_right = createbul Trail position 
        (rotate_deg scaled_vector (float_of_int (360 - cTRAIL_ANGLE))) 
        acc col in
      trail targ acc col (tracker + 1) 
        (b_straight::b_left::b_right::storage) position
  let deploy col arg pos = match arg with
    | Shoot (typ,bpos,accel) -> begin match typ with
      | Spread -> 
        (* Check edge case *)
        if cSPREAD_NUM <= 0 then []
        (* Create new spread bullets *)
        else spread bpos accel col 0.0 [] pos
      | Bubble ->
        (* Create new bubble bullet *)
        bubble bpos accel col pos
      | Trail ->
        (* Check edge case *)
        if cTRAIL_NUM <= 0 then []
        (* Create new trail bullets *)
        else trail bpos accel col 0 [] pos
      | Power -> [] (* Unavailable bullet type *) end
    | _ -> failwith "Invalid weapons command. Report to your superior!"
  (* Move Bullet - Update pos. and accel. Dodge this Neo! 
   * If still in field, then Some bullet else return None *)
  let move_bullet bul = 
    let new_pos = add_v bul.b_pos bul.b_vel in
    let new_vel = add_v bul.b_vel bul.b_accel in
    if (not (in_bounds new_pos)) 
    then 
      let _ = add_update(DeleteBullet (bul.b_id)) in
      None
    else
      let newbul = {bul with b_pos = new_pos} in
      add_update(MoveBullet(newbul.b_id, newbul.b_pos));
      if bul.b_type = Power then Some newbul
      else Some {newbul with b_vel = new_vel}
  let batch_bullets blist =
    List.fold_left (fun acc bul ->
      match move_bullet bul with
      | Some abul -> abul::acc
      | None -> acc) [] blist
  let remove_bullets blist =
    let _ = List.rev_map (fun bul -> 
      add_update(DeleteBullet (bul.b_id))) blist in
    [] 
end