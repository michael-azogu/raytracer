type ray =
  { source : Point.point
  ; direction : Vector.vector
  }

let make (source, direction) = { source; direction }

let at t { source; direction } =
  Point.translate source (Vector.scale direction ~by:t)
;;
