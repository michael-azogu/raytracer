(* [@@@ocaml.warning "-32"] *)

open Ppm

let scene_w = 556
let scene_h = 856

let () =
  let open Int in
  write_header ~w:scene_w ~h:scene_h;
  let render () =
    for j = 0 to scene_h - 1 do
      for i = 0 to scene_w - 1 do
        let r = to_float i /. to_float (scene_w - 1) in
        let g = to_float j /. to_float (scene_h - 1) in
        let b = 0.4 in
        let ir = 235.9 *. r in
        let ig = 135.9 *. g in
        let ib = 155.9 *. b in
        write_pixel (ir, ig, ib)
      done;
      flush stdout (* write horizontal slice of image *);
      Printf.eprintf
        "\r progress: %.1f %%"
        (to_float j /. (to_float scene_h -. 1.) *. 100.0);
      flush stderr
    done;
    Printf.eprintf "\n rendered %d pixels \n" (scene_h * scene_w)
  in
  render ()
;;
