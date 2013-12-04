open Definitions
open Constants
open Util
open Netgraphics

(* Module signature to maintain data for a player (Red or Blue) *)
module type Player_Data = sig
  (* Initial Red Character *)
  val red_char : player_char
  (* Initial Blue Character *)
  val blue_char : player_char
  (* Refocus *)
  val refocus : player_char -> bool -> player_char
  (* Update player position *)
  val keep_moving : player_char -> direction*direction -> player_char
end

(* Player functions *)
module Player_Mechanics = struct
  (* create the character given a color and position *)
  let createpc pos col = 
    let pc = {
      p_id = next_available_id (); 
      p_pos = pos;
      p_focused = false; 
      p_radius = cHITBOX_RADIUS; 
      p_color = col
    } in
    add_update (AddPlayer (pc.p_id, pc.p_color, pc.p_pos));
    add_update (SetCharge (pc.p_color, 0));
    add_update (SetLives (pc.p_color, cINITIAL_LIVES));
    add_update (SetBombs (pc.p_color, cINITIAL_BOMBS));
    add_update (SetScore (pc.p_color, 0));
    add_update (SetPower (pc.p_color, 0));
    pc
  let red_char = 
    let pos = (0.125 *. float_of_int (cBOARD_WIDTH), 
      0.50 *. float_of_int (cBOARD_HEIGHT)) in   
    createpc pos Red
  let blue_char = 
    let pos = (0.875 *. float_of_int (cBOARD_WIDTH), 
      0.50 *. float_of_int (cBOARD_HEIGHT)) in  
    createpc pos Blue
  let refocus pl abool = {pl with p_focused = abool}
  let keep_moving player track =
    if player.p_focused then
      let path = vector_of_dirs track (float_of_int cFOCUSED_SPEED) in
      let new_pos = add_v path player.p_pos in
      if in_bounds new_pos then {player with p_pos = new_pos} 
      else player
    else
      let path = vector_of_dirs track (float_of_int cUNFOCUSED_SPEED) in
      let new_pos = add_v path player.p_pos in
      if in_bounds new_pos then {player with p_pos = new_pos} 
      else player
end

(* Module signature to maintain data for a team (Red or Blue) *)
module type Team_Data = sig
  (* Initial Red Team *)
  val base_red : team_data
  (* Initial Blue Team *)
  val base_blue : team_data
  (* Check for dead character *)
  val death_check : team_data -> bool
  (* Toggle character focused state *)
  val toggle_focus : bool -> team_data -> team_data
  (* Get team charge *)
  val get_charge : team_data -> int
  (* Remove charge from team *)
  val rem_charge : team_data -> int -> team_data
  (* Add the charge rate and power to create the new charge *)
  val add_charge : team_data -> team_data
  (* Get position of team character *)
  val locate_rambo : team_data -> position
  (* Check if player has bombs *)
  val war_ready : team_data -> bool
  (* Remove bombs from the team *)
  val disarm_bomber : team_data -> team_data
  (* Determines if game has ended *)
  val check_endgame : team_data -> team_data -> result
  (* Apply a single move to the team character from the move list *)
  val recruit_rambo : team_data -> (direction * direction) list -> team_data
  (* Remove the first move from a move list if possible *)
  val alter_orders : (direction * direction) list -> 
    (direction * direction) list 
  (* Return player character *)
  val find_rambo : team_data -> player_char
  (* Add specified number of points to the team *)
  val point_count : team_data -> int -> team_data
  (* Add kill points to team *)
  val award_medal : team_data -> team_data
  (* Resets team data after suffering a bullet hit *)
  val reset_bullet_hit : team_data -> team_data
  (* Takes two ints - determines if invincible or not *)
  val protection : int -> int -> bool
  (* Adds power to the team *)
  val arm_rambo : team_data -> int -> team_data
end

(* Team functions *)
module Team_Mechanics : Team_Data = struct
  let base_red : team_data = 
    (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,Player_Mechanics.red_char)
  let base_blue : team_data = 
    (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,Player_Mechanics.blue_char)
  let death_check (l,b,s,p,c,pl) = (l = 0)
  let toggle_focus abool (l,q,s,p,c,pl) = 
    let npl = Player_Mechanics.refocus pl abool in
    (l,q,s,p,c,npl)
  let get_charge (l,q,s,p,c,pl) = c
  let rem_charge (l,q,s,p,c,pl) anint = 
    let newc = c - anint in
    let _ = add_update(SetCharge(pl.p_color, newc)) in
    (l,q,s,p,newc,pl)
  let add_charge (l,q,s,p,c,pl) = 
    let newc = c + p + cCHARGE_RATE in
    if newc > cCHARGE_MAX 
    then
      let _ = add_update(SetCharge(pl.p_color, cCHARGE_MAX)) in
      (l,q,s,p,cCHARGE_MAX,pl)
    else
      let _ = add_update(SetCharge(pl.p_color, newc)) in
      (l,q,s,p,newc,pl)
  let locate_rambo (l,q,s,p,c,pl) = pl.p_pos
  let war_ready (l,q,s,p,c,pl) = q > 0
  let disarm_bomber (l,q,s,p,c,pl) = 
    let newq = q - 1 in
    let _ = add_update(SetBombs(pl.p_color, newq)) in
    let _ = add_update(UseBomb(pl.p_color)) in
    (l,newq,s,p,c,pl)
  let check_endgame (l1,q1,s1,p1,c1,pl1) (l2,q2,s2,p2,c2,pl2) = 
    let x = (l1,q1,s1,p1,c1,pl1) in
    let y = (l2,q2,s2,p2,c2,pl2) in
    if death_check x && death_check y then
      if s1 > s2 then Winner pl1.p_color
      else if s2 > s1 then Winner pl2.p_color
      else Tie
    else if death_check x then Winner pl2.p_color
    else if death_check y then Winner pl1.p_color
    else Unfinished
  let recruit_rambo (l,q,s,p,c,pl) = function
    | [] -> (l,q,s,p,c,pl)
    | head::tail ->
      let npl = Player_Mechanics.keep_moving pl head in
      add_update (MovePlayer (npl.p_id, npl.p_pos));
      (l,q,s,p,c,npl)
  let alter_orders = function
    | [] -> []
    | head::tail -> tail
  let find_rambo (l,q,s,p,c,pl) = pl
  let point_count (l,q,s,p,c,pl) anint = 
    let news = s + anint in
    add_update(SetScore(pl.p_color, news));
    (l,q,news,p,c,pl)
  let award_medal ateam = point_count ateam cKILL_POINTS
  let reset_bullet_hit (l,q,s,p,c,pl) = 
    let newl = l - 1 in
    let newq = cINITIAL_BOMBS in
    let newp = p/2 in
    add_update(SetLives(pl.p_color, newl));
    add_update(SetBombs(pl.p_color, newq));
    add_update(SetPower(pl.p_color, newp));
    (newl,newq,s,newp,c,pl)
  let protection x y = x > 0 || y > 0
  let arm_rambo (l,q,s,p,c,pl) anint = 
    let newp = p + anint in
    let _ = add_update(SetPower(pl.p_color, newp)) in
    (l,q,s,newp,c,pl)
end