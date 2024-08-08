type point =
  { x : float
  ; y : float
  ; z : float
  }

let make (x, y, z) = { x; y; z }

let translate { x; y; z } ({ dx; dy; dz } : Vector.vector) =
  make (x +. dx, y +. dy, z +. dz)
;;

let displacement ~s:{ x; y; z } ~d:{ x = x'; y = y'; z = z' } =
  Vector.make (x -. x', y -. y', z -. z')
;;

let neg { x; y; z } = { x = -.x; y = -.y; z = -.z }
