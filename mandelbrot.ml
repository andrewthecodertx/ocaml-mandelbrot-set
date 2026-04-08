open Tsdl

let width  = 800
let height = 600

let max_iteration = 1000

let iterations x0 y0 =
  let c = { Complex.re = x0; im = y0 } in
  let z = ref Complex.zero in
  let iter = ref 0 in
  while !iter < max_iteration && (!z).Complex.re *. (!z).Complex.re +. (!z).Complex.im *. (!z).Complex.im <= 4.0 do
    z := Complex.add (Complex.mul !z !z) c;
    incr iter
  done;
  !iter

let color_of_iter n =
  if n = max_iteration then
    0xFF000000l
  else
    let v = int_of_float (255.0 *. float n /. float max_iteration) in
    Int32.of_int (0xFF000000 lor (v lsl 16) lor ((v / 2) lsl 8) lor (255 - v))

let zoom_factor = 2.0
let pan_fraction = 0.25

let pixel_to_complex px py xmin xmax ymin ymax =
  let x = xmin +. (float px /. float (width - 1)) *. (xmax -. xmin) in
  (* SDL y=0 is top, so invert *)
  let y = ymax -. (float py /. float (height - 1)) *. (ymax -. ymin) in
  (x, y)

let draw_mandelbrot pixels pitch xmin xmax ymin ymax =
  for sy = 0 to height - 1 do
    (* SDL row sy=0 is top of window = ymax in complex plane *)
    let y0 = ymax -. (float sy /. float (height - 1)) *. (ymax -. ymin) in
    for sx = 0 to width - 1 do
      let x0 = xmin +. (float sx /. float (width - 1)) *. (xmax -. xmin) in
      let it = iterations x0 y0 in
      pixels.{sy * pitch + sx} <- color_of_iter it
    done
  done

let zoom_at cx cy xmin xmax ymin ymax factor =
  let half_w = (xmax -. xmin) /. factor /. 2.0 in
  let half_h = (ymax -. ymin) /. factor /. 2.0 in
  (cx -. half_w, cx +. half_w, cy -. half_h, cy +. half_h)

let pan xmin xmax ymin ymax dx dy =
  let w = xmax -. xmin in
  let h = ymax -. ymin in
  let sx = dx *. w *. pan_fraction in
  let sy = dy *. h *. pan_fraction in
  (xmin +. sx, xmax +. sx, ymin +. sy, ymax +. sy)

let or_exit = function
  | Ok x -> x
  | Error (`Msg e) -> Printf.eprintf "SDL error: %s\n" e; exit 1

let redraw renderer texture xmin xmax ymin ymax =
  let (pixels, pitch) =
    Sdl.lock_texture texture None Bigarray.int32 |> or_exit
  in
  draw_mandelbrot pixels pitch xmin xmax ymin ymax;
  Sdl.unlock_texture texture;
  Sdl.render_clear renderer |> ignore;
  Sdl.render_copy renderer texture |> ignore;
  Sdl.render_present renderer

(* Shader emulation solution suggested by sol.vin (https://itch.io/profile/sol-vin) *)
(* Takes normalized coordinates (0.0 to 1.0) and returns color, shader-style *)
let simple_shader x y time =
  let r = sin (x *. 6.28 +. time) *. 0.5 +. 0.5 in
  let g = cos (y *. 6.28 +. time *. 1.5) *. 0.5 +. 0.5 in
  let b = sin ((x +. y) *. 3.14 +. time *. 0.8) *. 0.5 +. 0.5 in
  Int32.of_int ((int_of_float (r *. 255.0) lsl 16) lor
                (int_of_float (g *. 255.0) lsl 8) lor
                (int_of_float (b *. 255.0)) lor 0xFF000000)

(* Pixel shader implementation - processes each pixel independently (sol.vin) *)
let draw_shader pixels pitch time =
  for sy = 0 to height - 1 do
    let y = float sy /. float (height - 1) in
    for sx = 0 to width - 1 do
      let x = float sx /. float (width - 1) in
      pixels.{sy * pitch + sx} <- simple_shader x y time
    done
  done

let redraw_shader renderer texture time =
  let (pixels, pitch) =
    Sdl.lock_texture texture None Bigarray.int32 |> or_exit
  in
  draw_shader pixels pitch time;
  Sdl.unlock_texture texture;
  Sdl.render_clear renderer |> ignore;
  Sdl.render_copy renderer texture |> ignore;
  Sdl.render_present renderer

let () =
  Sdl.init Sdl.Init.video |> or_exit;
  let window =
    Sdl.create_window "Mandelbrot (OCaml)"
      ~w:width ~h:height Sdl.Window.shown |> or_exit
  in
  let renderer =
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync)
      window |> or_exit
  in
  let texture =
    Sdl.create_texture renderer Sdl.Pixel.format_argb8888
      Sdl.Texture.access_streaming ~w:width ~h:height |> or_exit
  in
  let view = ref (-2.5, 1.0, -1.25, 1.25) in
  (* Shader mode toggle functionality (sol.vin) *)
  let shader_mode = ref false in
  let xmin, xmax, ymin, ymax = !view in
  let start_time = Sdl.get_ticks () in
  redraw renderer texture xmin xmax ymin ymax;
  let event = Sdl.Event.create () in
  let running = ref true in
  while !running do
    match Sdl.wait_event (Some event) with
    | Error (`Msg e) ->
      Printf.eprintf "Event error: %s\n" e;
      running := false
    | Ok () ->
      let typ = Sdl.Event.(get event typ) in
      if typ = Sdl.Event.quit then
        running := false
      else if typ = Sdl.Event.mouse_button_down then begin
        let mx = Sdl.Event.(get event mouse_button_x) in
        let my = Sdl.Event.(get event mouse_button_y) in
        let xmin, xmax, ymin, ymax = !view in
        let cx, cy = pixel_to_complex mx my xmin xmax ymin ymax in
        view := zoom_at cx cy xmin xmax ymin ymax zoom_factor;
        let xmin, xmax, ymin, ymax = !view in
        if !shader_mode then
          let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
          redraw_shader renderer texture time
        else
          redraw renderer texture xmin xmax ymin ymax
      end
      else if typ = Sdl.Event.key_down then begin
        let key = Sdl.Event.(get event keyboard_keycode) in
        let xmin, xmax, ymin, ymax = !view in
        let _, (mx, my) = Sdl.get_mouse_state () in
        if key = Sdl.K.q || key = Sdl.K.escape then
          running := false
        else if key = Sdl.K.equals || key = Sdl.K.kp_plus then begin
          let cx, cy = pixel_to_complex mx my xmin xmax ymin ymax in
          view := zoom_at cx cy xmin xmax ymin ymax zoom_factor;
          let xmin, xmax, ymin, ymax = !view in
          if !shader_mode then
            let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
            redraw_shader renderer texture time
          else
            redraw renderer texture xmin xmax ymin ymax
        end
        else if key = Sdl.K.minus || key = Sdl.K.kp_minus then begin
          let cx, cy = pixel_to_complex mx my xmin xmax ymin ymax in
          view := zoom_at cx cy xmin xmax ymin ymax (1.0 /. zoom_factor);
          let xmin, xmax, ymin, ymax = !view in
          if !shader_mode then
            let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
            redraw_shader renderer texture time
          else
            redraw renderer texture xmin xmax ymin ymax
        end
        (* 't' key toggles between Mandelbrot and shader modes (sol.vin) *)
        else if key = Sdl.K.t then begin
          shader_mode := not !shader_mode;
          if !shader_mode then
            let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
            redraw_shader renderer texture time
          else
            redraw renderer texture xmin xmax ymin ymax
        end
        else if key = Sdl.K.r then begin
          view := (-2.5, 1.0, -1.25, 1.25);
          let xmin, xmax, ymin, ymax = !view in
          if !shader_mode then
            let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
            redraw_shader renderer texture time
          else
            redraw renderer texture xmin xmax ymin ymax
        end
        else begin
          let new_view =
            if key = Sdl.K.h || key = Sdl.K.a || key = Sdl.K.left then
              Some (pan xmin xmax ymin ymax (-1.0) 0.0)
            else if key = Sdl.K.l || key = Sdl.K.d || key = Sdl.K.right then
              Some (pan xmin xmax ymin ymax 1.0 0.0)
            else if key = Sdl.K.j || key = Sdl.K.s || key = Sdl.K.down then
              Some (pan xmin xmax ymin ymax 0.0 (-1.0))
            else if key = Sdl.K.k || key = Sdl.K.w || key = Sdl.K.up then
              Some (pan xmin xmax ymin ymax 0.0 1.0)
            else
              None
          in
          match new_view with
           | Some v ->
             view := v;
             let xmin, xmax, ymin, ymax = !view in
             if !shader_mode then
                let time = (float (Int32.to_int (Sdl.get_ticks ()) - Int32.to_int start_time)) /. 1000.0 in
               redraw_shader renderer texture time
             else
               redraw renderer texture xmin xmax ymin ymax
          | None -> ()
        end
      end
  done;
  Sdl.destroy_texture texture;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit ()
