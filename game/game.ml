open Definitions
open Constants
open Util
open MUfos
open MTeams
open MWeapons
open MCollisions

(* Record for holding game information.
 * Includes Red/blue teams, Ufos, Bullets, Powerups, Time Elapsed,
 * Pending Red Moves and Pending blue Moves. Store Ufos as tuple
 * to include creation time to track when to change direction.
 * Invincibilities stored separately for each team to track times. 
 * Also store bool to track if player was hit *)
type game = {mutable redx : team_data; mutable bluex : team_data; 
  mutable ufox : (ufo*int) list; mutable bl : bullet list; 
  mutable pl : power list; mutable time_el : int; 
  mutable rm : (direction * direction) list; 
  mutable bm : (direction * direction) list;
  mutable mri : int;
  mutable mbi : int;
  mutable bri : int;
  mutable bbi : int;}

(* Initializes and returns starting game state *)
let init_game () : game = 
  {redx = Team_Mechanics.base_red; 
    bluex = Team_Mechanics.base_blue; ufox = []; 
    bl = []; pl = []; time_el = 0; rm = []; bm = [];
    mri = 0; mbi = 0;
    bri = 0; bbi = 0;}

(* Updates the game state by a single time step *)
let handle_time game = 
  (* Continually edit parse_game for the current time_step *)
  let parse_game = game in

  (* Update current ufo positions (and velocities if needed) *)
  parse_game.ufox <- 
    UFO_Mechanics.batch_ufo parse_game.ufox parse_game.time_el;

  (* Spawn a new UFO if needed *)
  parse_game.ufox <- 
    if parse_game.time_el > 0 && 
      parse_game.time_el mod cUFO_SPAWN_INTERVAL = 0 then
      (UFO_Mechanics.base_ufo (),parse_game.time_el)::parse_game.ufox
    else parse_game.ufox;

  (* Update current bullet positions and velocities - removing those
   * that go off the field. *)
  parse_game.bl <- Weapon_Mechanics.batch_bullets parse_game.bl;

  (* Update powerup positions - removing those that leave the field *)
  parse_game.pl <- Weapon_Mechanics.batch_powerups parse_game.pl;

  (* Update player positions and player move lists*)
  parse_game.redx <- 
    Team_Mechanics.recruit_rambo parse_game.redx parse_game.rm;
  parse_game.rm <- Team_Mechanics.alter_orders parse_game.rm;
  parse_game.bluex <- 
    Team_Mechanics.recruit_rambo parse_game.bluex parse_game.bm;
  parse_game.bm <- Team_Mechanics.alter_orders parse_game.bm;

  (* Decrement invincibilities by one if greater than zero *)
  parse_game.mri <- 
    Collision_Mechanics.weaken_shield parse_game.mri;
  parse_game.mbi <- 
    Collision_Mechanics.weaken_shield parse_game.mbi;
  parse_game.bri <-
    Collision_Mechanics.weaken_shield parse_game.bri;
  parse_game.bbi <-
    Collision_Mechanics.weaken_shield parse_game.bbi;

  (* Check for red team hit by a bullet *)
  parse_game.redx <-
    if Team_Mechanics.protection parse_game.mri parse_game.bri then
    parse_game.redx
    else if Collision_Mechanics.valid_hit parse_game.bl 
      (Team_Mechanics.find_rambo parse_game.redx) then
      (fun () -> 
      	parse_game.mri <- cINVINCIBLE_FRAMES;
        parse_game.redx <- Team_Mechanics.reset_bullet_hit parse_game.redx;
        parse_game.bluex <- Team_Mechanics.award_medal parse_game.bluex;
        parse_game.redx) ()
    else parse_game.redx;

  (* Check for blue team hit by a bullet *)
  parse_game.bluex <-
    if Team_Mechanics.protection parse_game.mbi parse_game.bbi  then
    parse_game.bluex
    else if Collision_Mechanics.valid_hit parse_game.bl 
      (Team_Mechanics.find_rambo parse_game.bluex) then
      (fun () -> 
      	parse_game.mbi <- cINVINCIBLE_FRAMES;
        parse_game.bluex <- Team_Mechanics.reset_bullet_hit parse_game.bluex;
        parse_game.redx <- Team_Mechanics.award_medal parse_game.redx;
        parse_game.bluex) ()
    else parse_game.bluex;

  (* Update ufo hitcounts from bullet collisions*) 
  parse_game.ufox <- Collision_Mechanics.ufo_test
    parse_game.ufox parse_game.bl;

  (* Remove bullets that hit ufos *)
  parse_game.bl <- Collision_Mechanics.unhit_bullets 
    parse_game.ufox parse_game.bl;

  (* Scatter powerups for all destroyed ufos *)
  (* TODO *)

  (* Delete ufos that have been destroyed *)
  (* TODO *)
  
  (* If player - bullet collision with no invincibility, 
   * remove all bullets from the game *)
  parse_game.bl <- 
    if (Collision_Mechanics.valid_hit parse_game.bl 
      (Team_Mechanics.find_rambo parse_game.redx) && 
      not (Team_Mechanics.protection parse_game.mri parse_game.bri)) || 
      (Collision_Mechanics.valid_hit parse_game.bl  
      (Team_Mechanics.find_rambo parse_game.bluex) && 
      not (Team_Mechanics.protection parse_game.mbi parse_game.bbi)) then
      [] else parse_game.bl; 
    
  (* Check for red player - powerup collisions - add power *)
  parse_game.redx <- Team_Mechanics.arm_rambo parse_game.redx 
    (Collision_Mechanics.power_count parse_game.pl 
      (Team_Mechanics.find_rambo parse_game.redx));

  (* Check for blue player - powerup collisions - add power *)
  parse_game.bluex <- Team_Mechanics.arm_rambo parse_game.bluex 
    (Collision_Mechanics.power_count parse_game.pl 
      (Team_Mechanics.find_rambo parse_game.bluex));

  (* Remove powerups from field that collide with player *)
  parse_game.pl <- Collision_Mechanics.unhit_powers parse_game.pl 
    (Team_Mechanics.find_rambo parse_game.redx);

  parse_game.pl <- Collision_Mechanics.unhit_powers parse_game.pl 
    (Team_Mechanics.find_rambo parse_game.bluex);

  (* Increase time counter *)
  parse_game.time_el <- parse_game.time_el + 1;

  (* Check for endgame conditions - Return game * result *)
  let game_status = 
    Team_Mechanics.check_endgame parse_game.redx parse_game.bluex in
  (parse_game,game_status)

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
      n.bri <- cBOMB_DURATION; (* Add invincibility *)
      n   
    else gamma
  else
    if Team_Mechanics.war_ready gamma.bluex then 
      let n = gamma in
      n.bl <- []; (* Remove all bullets *)
      n.bluex <- Team_Mechanics.disarm_bomber n.bluex; (* Remove bomb *)
      n.bbi <- cBOMB_DURATION; (* Add invincibility *)
      n 
    else gamma

(* Return game data to bots *)
let get_data game = 
  let get_ufos l = 
    List.fold_left (fun a b -> (fst b)::a) [] l in
  (game.redx, game.bluex, get_ufos (game.ufox), game.bl, game.pl)

