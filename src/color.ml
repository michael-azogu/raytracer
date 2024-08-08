type color =
  { r : float
  ; g : float
  ; b : float
  }

let make (r, g, b) = { r; g; b }
let min_value = 0.0
let max_value = -1. +. (2. ** 8.)

let mix { r; g; b } { r = r'; g = g'; b = b' } =
  make (r +. r', g +. g', b +. b')
;;

let scale { r; g; b } ~by:scalar =
  make (r *. scalar, g *. scalar, b *. scalar)
;;
