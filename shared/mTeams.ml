open Definitions
open Constants
open Util

(* Module signature to maintain data for a player (Red or Blue) *)
module type Player_Data = sig
  (* Check to see if a point is outside of field *)
  val is_impassable : vector -> bool
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
  let is_impassable x = 
    let x_val = int_of_float (fst x) in 
    let y_val = int_of_float (snd x) in
    x_val >= 0 && x_val <= cBOARD_WIDTH && 
    y_val >= 0 && y_val <= cBOARD_HEIGHT
  let red_char = 
    let pos = (0.125 *. float_of_int (cBOARD_WIDTH), 
      0.50 *. float_of_int (cBOARD_HEIGHT)) in   
      {p_id = next_available_id (); p_pos = pos; p_focused = false; 
        p_radius = cHITBOX_RADIUS; p_color = Red}
  let blue_char = 
    let pos = (0.875 *. float_of_int (cBOARD_WIDTH), 
      0.50 *. float_of_int (cBOARD_HEIGHT)) in  
      {p_id = next_available_id (); p_pos = pos; p_focused = false; 
        p_radius = cHITBOX_RADIUS; p_color = Blue} 
  let refocus x b = {x with p_focused = b}
  let keep_moving player track =
    if player.p_focused then
      let path = vector_of_dirs track (float_of_int cFOCUSED_SPEED) in
      let new_pos = add_v path player.p_pos in
      if is_impassable new_pos then player
      else {player with p_pos = new_pos}
    else
      let path = vector_of_dirs track (float_of_int cUNFOCUSED_SPEED) in
      let new_pos = add_v path player.p_pos in
      if is_impassable new_pos then player
      else {player with p_pos = new_pos}
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
  (* Get position of team character *)
  val locate_rambo : team_data -> position
  (* Check if player has bombs *)
  val war_ready : team_data -> bool
  (* Remove bombs from the team *)
  val disarm_bomber : team_data -> team_data
  (* Determines if game has ended *)
  val check_endgame : team_data -> team_data -> result
end

(* Team functions *)
module Team_Mechanics : Team_Data = struct
  let base_red : team_data = 
    (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,Player_Mechanics.red_char)
  let base_blue : team_data = 
    (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,Player_Mechanics.blue_char)
  let death_check pl = match pl with 
  | (l,b,s,p,c,pl) -> if l = 0 then true else false
  let toggle_focus b t = match t with
  | (l,q,s,p,c,pl) -> 
    let npl = Player_Mechanics.refocus pl b in
    (l,q,s,p,c,npl)
  let get_charge t = match t with
  | (l,q,s,p,c,pl) -> c
  let rem_charge t i = match t with
  | (l,q,s,p,c,pl) -> (l,q,s,p,(c-i),pl)
  let locate_rambo i = match i with
  | (l,q,s,p,c,pl) -> pl.p_pos
  let war_ready i = match i with
  | (l,q,s,p,c,pl) -> if q > 0 then true else false
  let disarm_bomber i = match i with
  | (l,q,s,p,c,pl) -> (l,(q-1),s,p,c,pl)
  let check_endgame x y =
    match x with
    | (a,b,c,d,e,f) -> match y with
      | (g,h,i,j,k,l) -> 
        if death_check x && death_check y then
          if c > i then Winner f.p_color
          else if i > c then Winner l.p_color
          else Tie
        else if death_check x then Winner l.p_color
        else if death_check y then Winner f.p_color
        else Unfinished
end