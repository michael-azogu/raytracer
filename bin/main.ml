[@@@ocaml.warning "-32-33"]

open Raytracer

let aspect_ratio = 16.0 /. 9.0
let frame_w = 400

let frame_h =
  let h = Int.of_float (Float.of_int frame_w /. aspect_ratio) in
  if h < 1 then
    1
  else
    h
;;

let actual_ratio = Float.of_int frame_w /. Float.of_int frame_h
let vh = 2.0
let vw = vh *. actual_ratio
let focal_length = 1.0
let camera_center = Point.make (0., 0., 0.)
let vu = Vector.make (vw, 0., 0.)
let vv = Vector.make (0., -.vh, 0.)
let pixel_du = Vector.div vu (Int.to_float frame_w)
let pixel_dv = Vector.div vv (Int.to_float frame_h)

let viewport_upper_left =
  Point.translate
    camera_center
    (Vector.add
       (Vector.make (0., 0., focal_length))
       (Vector.neg (Vector.add (Vector.div vv 2.) (Vector.div vu 2.))))
;;

let pixel00_loc =
  Point.translate
    viewport_upper_left
    (Vector.scale (Vector.add pixel_du pixel_dv) ~by:0.5)
;;

let hit_sphere center radius ({ source; direction } : Ray.ray) =
  let oc = Point.displacement ~s:center ~d:(Point.neg source) in
  let a = Vector.dot direction direction in
  let b = -2.0 *. Vector.dot direction oc in
  let c = Vector.dot oc oc -. (radius ** 2.) in
  let discriminant = (b ** 2.) -. (4. *. a *. c) in
  if discriminant < 0. then
    -1.0
  else
    (-.b -. Float.sqrt discriminant) /. (2.0 *. a)
;;

let () =
  let shade ray =
    let t = hit_sphere (Point.make (0., 0., 1.)) 0.5 ray in
    if t > 0. then (
      let n =
        Vector.uv
          (Point.displacement
             ~s:(Ray.at t ray)
             ~d:(Point.make (0., 0., -1.)))
      in
      Color.scale
        (Color.make (n.dx +. 1., n.dy +. 1., n.dz +. 1.))
        ~by:0.5
    ) else (
      let uv = Vector.uv ray.direction in
      let a = 0.5 *. (uv.dy +. 1.) in
      Color.mix
        (Color.scale (Color.make (1.0, 1.0, 1.0)) ~by:(1.0 -. a))
        (Color.scale (Color.make (0.5, 0.7, 1.0)) ~by:a)
    )
  in
  let render () =
    let open Int in
    Ppm.write_header ~w:frame_w ~h:frame_h;
    for j = 0 to frame_h - 1 do
      for i = 0 to frame_w - 1 do
        let pixel_center =
          Point.translate
            pixel00_loc
            (Vector.add
               (Vector.scale pixel_du ~by:(to_float i))
               (Vector.scale pixel_dv ~by:(to_float j)))
        in
        let ray_direction =
          Point.displacement ~s:pixel_center ~d:camera_center
        in
        let ray = Ray.make (camera_center, ray_direction) in
        Ppm.write_pixel (shade ray)
      done;
      flush stdout;
      Printf.eprintf
        "\r progress: %.1f %%"
        (to_float j /. (to_float frame_h -. 1.) *. 100.0);
      flush stderr
    done;
    Printf.eprintf "\n rendered %d pixels \n" (frame_h * frame_w)
  in
  render ()
;;
