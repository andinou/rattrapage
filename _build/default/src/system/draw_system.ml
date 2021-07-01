open Component_defs

let ctx = ref None
let init () =
  let _, c = Gfx.create 
  "game_canvas:800x600:" 
  in
  ctx := Some c

let update _dt el =
  let ctx = Option.get !ctx in
  Gfx.clear_rect ctx 0 0 800 600;
  let player = Game_state.get_player() in
  let posp = Position.get player in
  let boxp = Box.get player in
  let xr = (posp.x+.((float_of_int boxp.width)/.2.))-.(800./.2.)in
  List.iter (fun e ->
    let pos = Position.get e in
    let box = Box.get e in
    
    let surface = Surface.get e in(* Question 3.2 *)
    match surface with
    Color color -> if(posp.x<(800.-.(float_of_int boxp.width))/.2.) then 
                            Gfx.fill_rect ctx (int_of_float pos.x)
                         (int_of_float pos.y)
                          box.width
                          box.height
                          color
                    else if(posp.x>8000.-.((800.+.(float_of_int boxp.width))/.2.)) then
                        Gfx.fill_rect ctx (int_of_float (pos.x-.(8000.-.800.)))
                         (int_of_float pos.y)
                          box.width
                          box.height
                          color
                          else Gfx.fill_rect ctx (int_of_float (pos.x-.xr))
                         (int_of_float pos.y)
                          box.width
                          box.height
                          color

                          (* Question 3.3 *)
    | Image render -> if(posp.x<(800.-.(float_of_int boxp.width))/.2.) then 
                            Gfx.blit_scale ctx render (int_of_float pos.x)
                         (int_of_float pos.y)
                          box.width
                          box.height
                    else if(posp.x>8000.-.((800.+.(float_of_int boxp.width))/.2.)) then
                        Gfx.blit_scale ctx render (int_of_float (pos.x-.(8000.-.800.)))
                         (int_of_float pos.y)
                          box.width
                          box.height
                          else Gfx.blit_scale ctx render (int_of_float (pos.x-.xr))
                         (int_of_float pos.y)
                          box.width
                          box.height
    | Animation anim -> (* Question 4.5 *)
                let v = Velocity.get e in
                let d = if v.x < 0.0 then -1 else if v.x > 0.0 then 1 else 0 in
                let render = Texture.get_frame anim d in
                Gfx.blit_scale ctx render (int_of_float pos.x)
                (int_of_float pos.y) box.width box.height
    | Text (text, font, color) ->
           Gfx.draw_text ctx text (int_of_float pos.x) (int_of_float pos.y) font color
    ) el;
    Gfx.commit ctx
