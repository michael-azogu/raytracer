open Stdio
open Vector

type point =
  { x : float
  ; y : float
  ; z : float
  }

let make (x, y, z) = { x; y; z }

let translate { x; y; z } { dx; dy; dz } =
  make (x +. dx, y +. dy, z +. dz)
;;
