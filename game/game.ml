open Definitions
open Constants
open Util
open MUfos
open MTeams
open MWeapons

(* Record for holding game information.
 * Includes Red/blue teams, Ufos, Bullets, Powerups, Time Elapsed,
 * Pending Red Moves and Pending blue Moves. *)
type game = {mutable redx : team_data; mutable bluex : team_data; 
  mutable ufox : (ufo*int) list; mutable bl : bullet list; 
  mutable pl : power list; mutable time_el : int; 
  mutable rm : (direction * direction) list; 
  mutable bm : (direction * direction) list;
  mutable red_invincibility : int;
  mutable blue_invincibility : int}

(* Initializes and returns starting game state *)
let init_game () : game = 
  {redx = Team_Mechanics.base_red; 
    bluex = Team_Mechanics.base_blue; ufox = []; 
    bl = []; pl = []; time_el = 0; rm = []; bm = [];
    red_invincibility = 0; blue_invincibility = 0}
  
(* Updates the game state by a single time step *)
let handle_time game =
  (* Continually edit parse_game for the current time_step *)
  (* let parse_game = game in *)
  (* Update UFO positions and velocities (if needed) *)

  (* Toggle player focus *)

  (* Update player positions *)

  (* Check for player and bullet collisions *)

  (* Check for UFO and bullet collisions *)

  (* Check for endgame conditions *)
  (* if Team_Mechanics.death_check game.redx && 
    Team_Mechanics.death_check game.bluex then *)
  failwith "todo"

(* Handles commands from the AIs - immediately update game state *)
let handle_action gamma col act = match act with
(* Update move list *)
| Move d -> 
  if col = Red then {gamma with rm = d}
  else {gamma with bm = d}
(* Handle shot request *)
| Shoot (b,c,d) -> 
  if col = Red then
    (* If Red has enough charge, get bullet list, add bullets to game,
     * subtract cost *)
    if Team_Mechanics.get_charge gamma.redx >= cost_of_bullet b then
      let location = Team_Mechanics.locate_rambo gamma.redx in
      let r = Weapon_Mechanics.deploy col act location in
      let n = gamma in
      n.redx <- Team_Mechanics.rem_charge n.redx (cost_of_bullet b);
      n.bl <- (List.append r (n.bl)); n
    else gamma
  else 
    (* If Blue has enough charge, get bullet list, add bullets to game,
     * subtract cost *)
    if Team_Mechanics.get_charge gamma.bluex >= cost_of_bullet b then
      let location = Team_Mechanics.locate_rambo gamma.bluex in
      let r = Weapon_Mechanics.deploy col act location in
      let n = gamma in
      n.bluex <- Team_Mechanics.rem_charge n.bluex (cost_of_bullet b);
      n.bl <- (List.append r (n.bl)); n
    else gamma
(* Toggle focused state *)
| Focus a ->
  if col = Red then
    let new_red = Team_Mechanics.toggle_focus a gamma.redx in
    {gamma with redx = new_red}
  else
    let new_blue = Team_Mechanics.toggle_focus a gamma.bluex in
    {gamma with bluex = new_blue}
(* Set off a bomb - Kaboom! *)
| Bomb -> 
  if col = Red then
    if Team_Mechanics.war_ready gamma.redx then
      let n = gamma in
      n.bl <- []; (* Remove all bullets *)
      n.redx <- Team_Mechanics.disarm_bomber n.redx; (* Remove bomb *)
      n.red_invincibility <- cBOMB_DURATION; (* Add invincibility *)
      n   
    else gamma
  else
    if Team_Mechanics.war_ready gamma.bluex then 
      let n = gamma in
      n.bl <- []; (* Remove all bullets *)
      n.bluex <- Team_Mechanics.disarm_bomber n.bluex; (* Remove bomb *)
      n.blue_invincibility <- cBOMB_DURATION; (* Add invincibility *)
      n 
    else gamma

(* Return game data to bots *)
let get_data game = 
  let get_ufos l = 
    List.fold_left (fun a b -> (fst b)::a) [] l in
  (game.redx, game.bluex, get_ufos (game.ufox), game.bl, game.pl)

