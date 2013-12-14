open Team
open Definitions
open Constants
open Util

(* blank data to be updated by recieve_data *)
type gdata = {
  mutable redx : team_data;
  mutable bluex : team_data; 
  mutable bl : bullet list; 
  mutable mri : int;
  mutable mbi : int;
  mutable bri : int;
  mutable bbi : int;
}

let data :gdata = {
  redx = 
    (0,0,0,0,0,{p_id=0;p_pos=(0.,0.);p_focused=true;p_radius=0;p_color=Red}); 
  bluex = 
    (0,0,0,0,0,{p_id=0;p_pos=(0.,0.);p_focused=true;p_radius=0;p_color=Blue});  
  bl = [];
  mri = 0; 
  mbi = 0;
  bri = 0; 
  bbi = 0;
}

let olddata :gdata = {
  redx = 
    (0,0,0,0,0,{p_id=0;p_pos=(0.,0.);p_focused=true;p_radius=0;p_color=Red}); 
  bluex = 
    (0,0,0,0,0,{p_id=0;p_pos=(0.,0.);p_focused=true;p_radius=0;p_color=Blue});  
  bl = [];
  mri = 0; 
  mbi = 0;
  bri = 0; 
  bbi = 0;
}

(* [receive_data d] is called whenever a game update comes from the server.
 * It's up to you what to do with this update. *)
let receive_data (d : game_data) : unit =
  (olddata.redx <- data.redx);
  (olddata.bluex <- data.bluex);
  (olddata.bl <- data.bl);
  (olddata.mri <- data.mri);
  (olddata.mbi <- data.mbi);
  (olddata.bri <- data.bri);
  (olddata.bbi <- data.bbi);
  let (rt, bt, uf, blist, plist) = d in
  (data.redx <- rt);
  (data.bluex <- bt);
  (data.bl <- blist);
  (if data.mri > 0 then data.mri <- data.mri - 1);
  (if data.mbi > 0 then data.mbi <- data.mbi - 1);
  (if data.bri > 0 then data.bri <- data.bri - 1);
  (if data.bbi > 0 then data.bbi <- data.bbi - 1)

let () = Random.self_init ()

let getSelfPos col =
  if col = Blue
  then 
    let (l,q,s,p,c,pl) = data.bluex in
    pl.p_pos
  else 
    let (l,q,s,p,c,pl) = data.redx in
    pl.p_pos
  
let getEnemyPos col =
  if col = Red
  then 
    let (l,q,s,p,c,pl) = data.bluex in
    pl.p_pos
  else 
    let (l,q,s,p,c,pl) = data.redx in
    pl.p_pos

let accelToEnemy col =
  let vec = subt_v (getEnemyPos col) (getSelfPos col) in
  let magn = cACCEL_LIMIT in
  scale magn (unit_v vec)

let hasCharge col btyp =
  if col = Blue
  then 
    let (l,q,s,p,c,pl) = data.bluex in
    c >= cost_of_bullet btyp
  else 
    let (l,q,s,p,c,pl) = data.redx in
    c >= cost_of_bullet btyp

let isCloseRange () =
  distance (getEnemyPos Blue) (getSelfPos Blue) <= 20.

let inGreatDanger col =
  let greatDangerBuls = List.filter 
    (fun bul -> bul.b_color <> col && 
      distance bul.b_pos (getSelfPos col) <= 
        float_of_int (cHITBOX_RADIUS + bul.b_radius + 1)) 
    data.bl in
  List.length greatDangerBuls > 0

let getDangerBuls col =
  List.filter (fun bul -> bul.b_color <> col && 
    distance (add_v bul.b_pos bul.b_vel) (getSelfPos col) <= 
      float_of_int (cHITBOX_RADIUS + bul.b_radius + 5)) 
    data.bl

let inDanger col =
  List.length (getDangerBuls col) > 0

let hasInvincibility col = 
  if col = Blue
  then (data.mbi + data.bbi) > 0
  else (data.mri + data.bri) > 0

let getDirection vec =
  let x = fst vec in
  let y = snd vec in
  if x > 0.
  then 
    if y > 0.
    then (North, East)
    else if y = 0.
    then (East, Neutral)
    else (South, East)
  else if x < 0.
  then 
    if y > 0.
    then (North, West)
    else if y = 0.
    then (West, Neutral)
    else (South, West)
  else if y > 0.
  then (North, Neutral)
  else if y < 0.
  then (South, Neutral)
  else (Neutral, Neutral)

let getCloser col =
  let vec = subt_v (getEnemyPos col) (getSelfPos col) in
  let direc = getDirection vec in
  [direc;direc;direc;direc]

let dodge col =
  let buls = getDangerBuls col in
  List.fold_left 
    (fun acc bul -> 
      let vec = subt_v (getSelfPos col) bul.b_pos in
      (getDirection vec)::acc) [] buls

let usedBomb col =
  if col = Red then data.bri <- cBOMB_DURATION
  else data.bbi <- cBOMB_DURATION

let checkHit col =
  if col = Red 
  then 
    let (l1,q1,s1,p1,c1,pl1) = data.redx in
    let (l2,q2,s2,p2,c2,pl2) = olddata.redx in
    if l1 > l2 then data.mri <- cINVINCIBLE_FRAMES
  else
    let (l1,q1,s1,p1,c1,pl1) = data.bluex in
    let (l2,q2,s2,p2,c2,pl2) = olddata.bluex in
    if l1 > l2 then data.mbi <- cINVINCIBLE_FRAMES

let bot col =
  while true do
    let () = checkHit col in
    let () = 
      if inGreatDanger col && not (hasInvincibility col)
      then 
        let _ = (usedBomb col) in
        send_action (Bomb)
      else if inDanger col && not (hasInvincibility col)
      then send_action (Move(dodge col)) 
      else send_action (Move(getCloser col)) in
    let () = 
      if hasCharge col Trail && not (isCloseRange ())
      then send_action (Shoot (Trail, getEnemyPos col, (accelToEnemy col))) 
      else if hasCharge col Spread && isCloseRange ()
      then send_action (Shoot (Spread, getEnemyPos col, (accelToEnemy col)))
      else () in
    Thread.delay cUPDATE_TIME
  done

let () = start_bot bot receive_data
