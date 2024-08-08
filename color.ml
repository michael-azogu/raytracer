type color =
  { r : float
  ; g : float
  ; b : float
  }

let make (r, g, b) = { r; g; b }
let min_value = 0.0
let max_value = -1. +. (2. ** 8.)
