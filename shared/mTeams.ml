open Definitions
open Constants
open Util

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
  (* Get position of team character *)
  val locate_rambo : team_data -> position
  (* Check if player has bombs *)
  val war_ready : team_data -> bool
  (* Remove bombs from the team *)
  val disarm_bomber : team_data -> team_data
  (* Determines if game has ended *)
  val check_endgame : team_data -> team_data -> result
  (* Apply a single move to the team character from the move list *)
  val recruit_rambo : team_data -> 
    (direction * direction) list -> team_data
  (* Remove the first move from a move list if possible *)
  val alter_orders : (direction * direction) list -> 
    (direction * direction) list 
  (* Return player character *)
  val find_rambo : team_data -> player_char
  (* Add kill points to team *)
  val award_medal : team_data -> team_data
  (* Resets team data after suffering a bullet hit *)
  val reset_bullet_hit : team_data -> team_data
  (* Takes two ints - determines if invincible or not *)
  val protection : int -> int -> bool
  (* Adds power to the team *)
  val arm_rambo : team_data -> int -> team_data
  (* Add specified number of points to the team *)
  val point_count : team_data -> int -> team_data
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
  let recruit_rambo t d = match t with
    | (l,q,s,p,c,pl) -> 
      begin match d with
      | [] -> (l,q,s,p,c,pl)
      | head::tail ->
        let npl = Player_Mechanics.keep_moving pl head in
        (l,q,s,p,c,npl) end
  let alter_orders d = match d with
    | [] -> []
    | head::tail -> tail
  let find_rambo x = match x with
    | (l,q,s,p,c,pl) -> pl
  let award_medal x = match x with
    | (l,q,s,p,c,pl) -> (l,q,(s + cKILL_POINTS),p,c,pl)
  let reset_bullet_hit x = match x with
    | (l,q,s,p,c,pl) -> ((l-1),cINITIAL_BOMBS,s,(p/2),c,pl)
  let protection x y = x > 0 || y > 0
  let arm_rambo x y = match x with
    | (l,q,s,p,c,pl) -> (l,q,s,(p+y),c,pl)
  let point_count t i = match t with
    | (l,q,s,p,c,pl) -> (l,q,(s+i),p,c,pl)
end