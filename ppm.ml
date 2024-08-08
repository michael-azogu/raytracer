open Base
open Stdio.Out_channel

exception Out_of_Bounds of string

let write_header ~w ~h =
  print_string
    (Printf.sprintf "P6\n%d %d\n%.0f\n" w h Color.max_value)
;;

let write_pixel (r, g, b) =
  let open Char in
  let open Float in
  if
    List.for_all
      ~f:(fun i -> between i ~low:0. ~high:Color.max_value)
      [ r; g; b ]
  then (
    let rgb = Bytes.create 3 in
    Bytes.set rgb 0 (of_int_exn (to_int r));
    Bytes.set rgb 1 (of_int_exn (to_int g));
    Bytes.set rgb 2 (of_int_exn (to_int b));
    output_bytes stdout rgb
  ) else
    raise (Out_of_Bounds "out of bounds RGB value")
;;
