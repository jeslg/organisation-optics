package dev.habla

// Embedding T-Linq using finally tagless
trait TLinq[Repr[_]] {

  def int(i: Int): Repr[Int]

  def bool(b: Boolean): Repr[Boolean]

  def string(s: String): Repr[String]

  def foreach[A, B](
    as: Repr[List[A]])(
    f: Repr[A] => Repr[List[B]]): Repr[List[B]]

  def where[A](c: Repr[Boolean])(as: Repr[List[A]]): Repr[List[A]]

  def yields[A](a: Repr[A]): Repr[List[A]]

  def tuple[A, B](a: Repr[A], b: Repr[B]): Repr[(A, B)]

  def nil[A]: Repr[List[A]]

  def subtract(x: Repr[Int], y: Repr[Int]): Repr[Int]

  def greaterThan(x: Repr[Int], y: Repr[Int]): Repr[Boolean]

  def equal[A](a1: Repr[A], a2: Repr[A]): Repr[Boolean]

  def and(p: Repr[Boolean], q: Repr[Boolean]): Repr[Boolean]

  def not(p: Repr[Boolean]): Repr[Boolean]

  def exists[A](f: Repr[List[A]]): Repr[Boolean]

  def ifs[A](b: Repr[Boolean], t: Repr[A], e: Repr[A]): Repr[A]

  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]

  def app[A, B](f: Repr[A => B])(a: Repr[A]): Repr[B]

  def some[A](a: Repr[A]): Repr[Option[A]]

  def none[A]: Repr[Option[A]]

  def ofold[A, B](oa: Repr[Option[A]])(z: Repr[B], f: Repr[A => B]): Repr[B]
}

object TLinq {

  // Standard semantics
  implicit object R extends TLinq[λ[x => x]] {

    def int(i: Int) = i

    def bool(b: Boolean) = b

    def string(s: String) = s

    def foreach[A, B](as: List[A])(f: A => List[B]): List[B] =
      as.flatMap(f)

    def where[A](c: Boolean)(as: List[A]): List[A] =
      if (c) as else nil

    def yields[A](a: A): List[A] = List(a)

    def tuple[A, B](a: A, b: B) = (a, b)

    def nil[A]: List[A] = List()

    def subtract(x: Int, y: Int) = x - y

    def greaterThan(x: Int, y: Int) = x > y

    def equal[A](a1: A, a2: A) = a1 == a2

    def and(p: Boolean, q: Boolean) = p && q

    def not(b: Boolean) = !b

    def exists[A](as: List[A]) = as.nonEmpty

    def ifs[A](c: Boolean, t: A, e: A) = if (c) t else e

    def lam[A, B](f: A => B) = f

    def app[A, B](f: A => B)(a: A) = f(a)

    def some[A](a: A) = Some(a)

    def none[A] = None

    def ofold[A, B](oa: Option[A])(z: B, f: A => B) = oa match {
      case None => z
      case Some(a) => f(a)
    }
  }
}

