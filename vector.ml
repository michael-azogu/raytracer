type vector =
  { dx : float
  ; dy : float
  ; dz : float
  }

let make (dx, dy, dz) = { dx; dy; dz }

let rec neg v = scale v ~by:(-1.)

and add { dx; dy; dz } { dx = dx'; dy = dy'; dz = dz' } =
  make (dx +. dx', dy +. dy', dz +. dz')

and sub { dx = dx'; dy = dy'; dz = dz' } ~from:{ dx; dy; dz } =
  make (dx -. dx', dy -. dy', dz -. dz')

and div v (scalar : float) = scale v ~by:(scalar ** -1.)

and scale { dx; dy; dz } ~by:scalar =
  make (scalar *. dx, scalar *. dy, scalar *. dz)

and ewp { dx; dy; dz } { dx = dx'; dy = dy'; dz = dz' } =
  make (dx *. dx', dy *. dy', dz *. dz')

and dot { dx; dy; dz } { dx = dx'; dy = dy'; dz = dz' } =
  (dx *. dx') +. (dy *. dy') +. (dz *. dz')

and cross { dx; dy; dz } { dx = dx'; dy = dy'; dz = dz' } =
  make
    ( (dy *. dz') -. (dz *. dy')
    , (dz *. dx') -. (dx *. dz')
    , (dx *. dy') -. (dy *. dx') )

and uv v = div v (magnitude v)
and magnitude v = Float.sqrt (msq v)
and msq { dx; dy; dz } = (dx ** 2.) +. (dy ** 2.) +. (dz ** 2.)
