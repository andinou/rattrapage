open Ecs
let chain_functions f_list =
  let funs = ref f_list in
  fun dt -> match !funs with
               [] -> false
              | f :: ll ->
                 if f dt then true
                 else begin
                  funs := ll;
                  true
                 end


let load_level _dt = 
	let ic = open_in "/static/files/level1.txt" in
	let () =
	try
		let count = ref 0 in
		while true do
			let line = input_line ic in
			match String.split_on_char ',' line with
			[ sx; sy; sw; sh ] -> ignore (Wall.create ("wall" ^ string_of_int !count)
														(float_of_string sx)
														(float_of_string sy)
														(int_of_string sw)
														(int_of_string sh));
														count := !count + 1;
			| _ ->  ()
		done
	with End_of_file -> ()
	in
	false

let init_player _dt =
	let player = Player.create "player" 100.0 300.0 in
    let _final = Final.create "final" player 7950.0 0.0 50 550 in
    let _piege = Piege.create "piege" player 400.0 400.0 100 50 in
	Game_state.init player;
	Input_handler.register_command (KeyDown "z") (Player.jump);
	Input_handler.register_command (KeyUp "z") (Player.stop_jump);
	
  Input_handler.register_command (KeyDown "q") (Player.run_left);
  Input_handler.register_command (KeyUp "q") (Player.stop_run_left);
  Input_handler.register_command (KeyDown "d") (Player.run_right);
  Input_handler.register_command (KeyUp "d") (Player.stop_run_right);
	System.init_all ();
	false

let play_game dt =
	Player.do_move ();
	System.update_all dt;
	true


let run () =
	Gfx.main_loop
	(chain_functions [
		load_level;
		init_player;
		play_game
	])
