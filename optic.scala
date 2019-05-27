package dev.habla

// We use our own structures for optics (instead of function aliases), because
// we want to provide a new semantic domain for each optic, in order to avoid
// conflicts with lambda terms.

case class Getter[S, A](get: S => A)

case class Affine[S, A](getOpt: S => Option[A])

case class Fold[S, A](getAll: S => List[A])

