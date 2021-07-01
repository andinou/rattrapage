open Ecs
open Component_defs
open System_defs

let resolve_collision self _other =
    let owner = Owner.get self in
    let p1 = Game_state.get_player () in
    Player.reset p1 32. 64.
  

let create name player x y w h =
  let e = Entity.create () in
  (* components *)
  Position.set e { x = x; y = y};
  Velocity.set e Vector.zero;
  Mass.set e infinity;
  Box.set e {width = w; height=h };
  Name.set e name;
  Owner.set e player;
  CollisionResolver.set e resolve_collision;
  Surface.set e Texture.red;
  (* Systems *)
  Collision_S.register e;
  Draw_S.register e;
  e
