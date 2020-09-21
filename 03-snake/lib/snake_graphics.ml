open! Base

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let body_colors =
    [ Graphics.rgb 000 255 000
    ; Graphics.rgb 255 255 051
    ; Graphics.rgb 255 178 102
    ; Graphics.rgb 255 102 102
    ; Graphics.rgb 000 255 255
    ; Graphics.rgb 224 224 224
    ]
  let head_color = Graphics.rgb 100 100 125
  let apple_color = Graphics.rgb 255 000 000
  let game_in_progress = Graphics.rgb 100 100 200
  let game_lost = Graphics.rgb 200 100 100
  let game_won = Graphics.rgb 100 200 100
end

module Constants = struct
  let play_area_height = 400
  let header_height = 50
  let play_area_width = 450
  let block_size = 18
  let text_size = 20
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one then failwith "Can only call init_exn once" else only_one := true;
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_height + header_height) play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Game.create ~height ~width ~initial_snake_length:3 ~amount_to_grow:3
;;

let draw_block { Position.row; col } ~color =
  let open Constants in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color color;
  Graphics.fill_rect (col + 1) (row + 1) (block_size - 1) (block_size - 1)
;;

let draw_text ~x ~y ~size ?(color=Colors.black) text =
  Graphics.set_color color;
  Graphics.set_text_size size;
  Graphics.moveto x y;
  Graphics.draw_string (Printf.sprintf " %s" text)

let draw_header ~game_state ~score =
  let open Constants in
  let header_color =
    match (game_state : Game_state.t) with
    | In_progress -> Colors.game_in_progress
    | Game_over _ -> Colors.game_lost
    | Win -> Colors.game_won
  in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let text_y = play_area_height + 25 in
  let header_text = Game_state.to_string game_state in
  draw_text ~x:0 ~y:text_y ~size:text_size header_text;
  let score_text = "Score: " ^ Score.to_string score in
  draw_text ~x:(play_area_width - 80) ~y:text_y ~size:text_size score_text
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_apple apple =
  let apple_location = Apple.location apple in
  draw_block apple_location ~color:Colors.apple_color
;;

let current_body_color = ref (List.nth_exn Colors.body_colors 0)

let random_body_color () =
  List.nth_exn Colors.body_colors (Random.int_incl 0 (List.length Colors.body_colors - 1))

let rec set_new_color current_color get_random_color =
  let random_color = get_random_color() in
  if current_color = random_color
  then set_new_color current_color get_random_color
  else random_color

let draw_snake snake_locations to_change_color =
  let body_color =
    if not to_change_color
    then !current_body_color
    else (
      current_body_color := set_new_color !current_body_color random_body_color;
      !current_body_color
    )
  in
  List.iter snake_locations ~f:(draw_block ~color:body_color);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head_color (List.hd_exn snake_locations)
;;

let render game =
  let snake = Game.snake game in
  let apple = Game.apple game in
  let game_state = Game.game_state game in
  let snake_locations = Snake.locations snake in
  let score = Game.score game in
  let to_change_color = Game.to_change_color game in
  draw_header ~game_state ~score;
  draw_play_area ();
  draw_apple apple;
  draw_snake snake_locations to_change_color;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

let read_key () = if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
