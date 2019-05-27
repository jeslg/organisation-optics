package dev.habla

case class Getter[S, A](get: S => A)

case class Affine[S, A](getOpt: S => Option[A])

case class Fold[S, A](getAll: S => List[A])

