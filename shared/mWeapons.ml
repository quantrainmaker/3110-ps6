open Definitions
open Constants
open Util
open MTeams

(* Handle weapon functionality - Bullets *)
module type Weapons = sig
  (* Return bullets list - Fire in the hole! *)
  val deploy : color -> action -> position -> bullet list
  (* Create list of bullets of Spread type *)
  val spread : vector -> vector -> color ->
    float -> bullet list -> position -> bullet list
  (* Create bullet of Bubble type *)
  val bubble : vector -> vector -> color -> position -> bullet list
  (* Create list of bullets of Trail type *)
  val trail : vector -> vector -> color -> 
    int -> bullet list -> position -> bullet list
  (* Check to see if a point is outside of field *)
  val is_impassable : vector -> bool
  (* Move Bullet - Update pos. and accel. Dodge this Neo! 
   * If still in field, then Some bullet else return None *)
  val metal_move : bullet -> bullet option
  (* Update list of bullets, removing those that are off the field *)
  val batch_bullets : bullet list -> bullet list
  (* Move the powerups's positions by their velocities *)
  val batch_powerups : power list -> power list
end

module Weapon_Mechanics : Weapons = struct
  let rec spread targ acc col tracker storage position = 
    if int_of_float tracker >= cSPREAD_NUM then storage
    else
      let base_vector = subt_v targ position in
      let scaled_vector = 
        scale (float_of_int cSPREAD_SPEED) (unit_v base_vector) in
      let interval = 2.0 *. pi /. (float_of_int cSPREAD_NUM) in
      let rotated_vel = rotate scaled_vector (tracker *. interval) in
      let valid_acc = if magnitude acc > cACCEL_LIMIT then 
        (0.0, 0.0) else acc in
      let nb = {b_type = Spread; b_id = next_available_id (); 
        b_pos = position; b_vel = rotated_vel; b_accel = valid_acc; 
        b_radius = cSPREAD_RADIUS; b_color = col} in
      spread targ acc col (tracker +. 1.0) (nb::storage) position
  let bubble targ acc col position = 
    let base_vector = subt_v targ position in
    let scaled_vector = 
        scale (float_of_int cBUBBLE_SPEED) (unit_v base_vector) in
    let valid_acc = if magnitude acc > cACCEL_LIMIT then 
        (0., 0.) else acc in
    let nb = {b_type = Bubble; b_id = next_available_id (); b_pos = position;
        b_vel = scaled_vector; b_accel = valid_acc; b_radius = cBUBBLE_RADIUS;
        b_color = col} in 
        [nb]
  let rec trail targ acc col tracker storage position = 
    if tracker >= cTRAIL_NUM then storage
    else
      let speed_factor = (tracker + 1) * cTRAIL_SPEED_STEP in
      let base_vector = subt_v targ position in
      let scaled_vector = 
        scale (float_of_int speed_factor) (unit_v base_vector) in
      let valid_acc = if magnitude acc > cACCEL_LIMIT then 
        (0., 0.) else acc in
      let b_straight = {b_type = Trail; b_id = next_available_id (); 
        b_pos = position; b_vel = scaled_vector; b_accel = valid_acc; 
        b_radius = cTRAIL_RADIUS; b_color = col} in 
      let b_left = {b_type = Trail; b_id = next_available_id (); 
        b_pos = position; b_vel = 
        (rotate_deg scaled_vector (float_of_int cTRAIL_ANGLE)); 
        b_accel = valid_acc; b_radius = cTRAIL_RADIUS; b_color = col} in
      let b_right = {b_type = Trail; b_id = next_available_id (); 
        b_pos = position; b_vel = (rotate_deg scaled_vector 
          (float_of_int (360 - cTRAIL_ANGLE))); 
        b_accel = valid_acc; b_radius = cTRAIL_RADIUS; b_color = col} in
      trail targ acc col (tracker + 1) 
        (b_straight::b_left::b_right::storage) position
  let deploy col arg pos = match arg with
    | Shoot (x,y,z) -> begin match x with
      | Spread -> 
        (* Check edge case *)
        if cSPREAD_NUM <= 0 then []
        (* Create new spread bullets *)
        else spread y z col 0.0 [] pos
      | Bubble ->
        (* Create new bubble bullet *)
        bubble y z col pos
      | Trail ->
        (* Check edge case *)
        if cTRAIL_NUM <= 0 then []
        (* Create new trail bullets *)
        else trail y z col 0 [] pos
      | Power -> [] (* Unavailable bullet type *) end
    | _ -> failwith "Invalid weapons command. Report to your superior!"
  let is_impassable x = 
    let x_val = fst x in 
    let y_val = snd x in
    x_val >= 0.0 && x_val <= float_of_int (cBOARD_WIDTH) && 
    y_val >= 0.0 && y_val <= float_of_int (cBOARD_HEIGHT)
  let metal_move x = 
    let new_pos = add_v x.b_pos x.b_vel in
    let new_vel = add_v x.b_vel x.b_accel in
    if is_impassable new_pos then None
    else 
      Some {b_type = x.b_type; b_id = x.b_id; b_pos = new_pos;
        b_vel = new_vel; b_accel = x.b_accel; b_radius = x.b_radius;
        b_color = x.b_color}
  let batch_bullets b =
    List.fold_left (fun a x ->
      match metal_move x with
      | Some y -> y::a
      | None -> a) [] b
  let batch_powerups x = 
    List.fold_left (fun acc y -> 
      let j = {y with b_vel = (add_v y.b_vel y.b_pos)} in
      j::acc) [] x
end