package dev

package object `habla` {

  type Getter[S, A] = S => A

  type Affine[S, A] = S => Option[A]

  type Fold[S, A] = S => List[A]

  type Org = List[Department]
}

