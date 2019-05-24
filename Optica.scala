package dev.habla

import Base._

trait Optica[Repr[_], Obs[_]] {

  /* Getter */
 
  def id_gt[S]: Repr[Getter[S, S]]

  def comp_gt[S, A, B](
    u: Repr[Getter[S, A]],
    d: Repr[Getter[A, B]]): Repr[Getter[S, B]]

  def fork_gt[S, A, B](
    l: Repr[Getter[S, A]],
    r: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]]

  def like[S, A: Base](a: A): Repr[Getter[S, A]]

  def not[S](b: Repr[Getter[S, Boolean]]): Repr[Getter[S, Boolean]]

  def equal[S, A: Base](
    x: Repr[Getter[S, A]],
    y: Repr[Getter[S, A]]): Repr[Getter[S, Boolean]]

  def greaterThan[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]]

  def subtract[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]]

  def get[S, A](gt: Repr[Getter[S, A]]): Obs[S => A]

  /* Affine */
  
  def id_af[S]: Repr[Affine[S, S]]

  def comp_af[S, A, B](
    u: Repr[Affine[S, A]],
    d: Repr[Affine[A, B]]): Repr[Affine[S, B]]

  def filtered[S](p: Repr[Getter[S, Boolean]]): Repr[Affine[S, S]]

  def as_af[S, A](gt: Repr[Getter[S, A]]): Repr[Affine[S, A]]

  def getOpt[S, A](af: Repr[Affine[S, A]]): Obs[S => Option[A]]

  /* Fold */

  def id_fl[S]: Repr[Fold[S, S]]

  def comp_fl[S, A, B](
    u: Repr[Fold[S, A]],
    d: Repr[Fold[A, B]]): Repr[Fold[S, B]]

  def nonEmpty[S, A](fl: Repr[Fold[S, A]]): Repr[Getter[S, Boolean]]

  def empty[S, A](fl: Repr[Fold[S, A]]): Repr[Getter[S, Boolean]] =
    not(nonEmpty(fl))

  def any[S, A](
      fl: Repr[Fold[S, A]])(
      p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
    nonEmpty(comp_fl(fl, as_fl(filtered(p))))

  def all[S, A](
      fl: Repr[Fold[S, A]])(
      p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
    empty(comp_fl(fl, as_fl(filtered(not(p)))))

  def elem[S, A: Base](fl: Repr[Fold[S, A]])(a: A): Repr[Getter[S, Boolean]] =
    any(fl)(equal(id_gt, like(a)))

  def as_fl[S, A](af: Repr[Affine[S, A]]): Repr[Fold[S, A]]

  def getAll[S, A](fl: Repr[Fold[S, A]]): Obs[S => List[A]]
}

object Optica {

  // Interprets Optica over TLinq
  implicit def tLinqOptica[Repr[_]](implicit 
      TL: TLinq[Repr]): Optica[Repr, Repr] = new Optica[Repr, Repr] {
    import TL._

    /* Getter */

    def comp_gt[S, A, B](u: Repr[Getter[S, A]], d: Repr[Getter[A, B]]) =
      lam(s => app(d)(app(u)(s)))

    def fork_gt[S, A, B](l: Repr[Getter[S, A]], r: Repr[Getter[S, B]]) = 
      lam(s => tuple(app(l)(s), app(r)(s)))

    def id_gt[S] = lam(identity)

    def like[S, A](a: A)(implicit B: Base[A]) = lam[S, A](_ =>
      B match {
        case IntWitness => int(a)
        case BooleanWitness => bool(a)
        case StringWitness => string(a)
      })

    def not[S](b: Repr[Getter[S, Boolean]]) = lam(s => TL.not(app(b)(s)))

    def equal[S, A: Base](x: Repr[Getter[S, A]], y: Repr[Getter[S, A]]) =
      lam(s => TL.equal(app(x)(s), app(y)(s)))

    def greaterThan[S](x: Repr[Getter[S, Int]], y: Repr[Getter[S, Int]]) = 
      lam(s => TL.greaterThan(app(x)(s), app(y)(s)))

    def subtract[S](x: Repr[Getter[S, Int]], y: Repr[Getter[S, Int]]) =
      lam(s => TL.subtract(app(x)(s), app(y)(s)))

    def get[S, A](gt: Repr[Getter[S, A]]) = gt

    /* Affine */

    def id_af[S] = lam(some)

    def comp_af[S, A, B](u: Repr[Affine[S, A]], d: Repr[Affine[A, B]]) =
      lam(s => ofold(app(u)(s))(none, lam(app(d))))

    def filtered[S](p: Repr[Getter[S, Boolean]]) =
      lam(s => ifs(app(p)(s), some(s), none))

    def as_af[S, A](gt: Repr[Getter[S, A]]) = lam(s => some(app(gt)(s)))

    def getOpt[S, A](af: Repr[Affine[S, A]]) = af

    /* Fold */

    def id_fl[S] = lam(yields)

    def comp_fl[S, A, B](u: Repr[Fold[S, A]], d: Repr[Fold[A, B]]) =
      lam(s => 
        foreach(app(u)(s))(a =>
          foreach(app(d)(a))(yields)))

    def nonEmpty[S, A](fl: Repr[Fold[S, A]]) = lam(s => exists(app(fl)(s)))

    def as_fl[S, A](af: Repr[Affine[S, A]]) =
      lam(s => ofold(app(af)(s))(nil, lam(yields)))

    def getAll[S, A](fl: Repr[Fold[S, A]]) = fl
  }

  trait Syntax {

    implicit class GetterOps[Repr[_], Obs[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev: Optica[Repr, Obs]) {

      def >>>[B](other: Repr[Getter[A, B]]): Repr[Getter[S, B]] =
        ev.comp_gt(gt, other)

      def ***[B](other: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]] =
        ev.fork_gt(gt, other)

      def get: Obs[S => A] = ev.get(gt)
    }

    implicit class GetterBaseOps[Repr[_], Obs[_], S, B: Base](
        gt: Repr[Getter[S, B]])(implicit
        ev: Optica[Repr, Obs]) {

      def ===(other: Repr[Getter[S, B]]): Repr[Getter[S, Boolean]] =
        ev.equal(gt, other)
    }

    implicit class GetterArithOps[Repr[_], Obs[_], S](
        gt: Repr[Getter[S, Int]])(implicit
        ev: Optica[Repr, Obs]) {

      def >(y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]] =
        ev.greaterThan(gt, y)

      def -(y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]] =
        ev.subtract(gt, y)
    }

    implicit def liftLike[Repr[_], Obs[_], S, B: Base](
        b: B)(implicit
        ev: Optica[Repr, Obs]): Repr[Getter[S, B]] =
      ev.like(b)

    implicit class AffineOps[Repr[_], Obs[_], S, A](
        af: Repr[Affine[S, A]])(implicit
        ev: Optica[Repr, Obs]) {

      def >>>[B](other: Repr[Affine[A, B]]): Repr[Affine[S, B]] =
        ev.comp_af(af, other)

      def getOpt: Obs[S => Option[A]] = ev.getOpt(af)
    }

    implicit def gt_as_af[Repr[_], Obs[_], S, A](
        af: Repr[Getter[S, A]])(implicit
        ev: Optica[Repr, Obs]): Repr[Affine[S, A]] =
      ev.as_af(af)

    implicit class FoldOps[Repr[_], Obs[_], S, A](
        fl: Repr[Fold[S, A]])(implicit 
        ev: Optica[Repr, Obs]) {

      def >>>[B](other: Repr[Fold[A, B]]): Repr[Fold[S, B]] = 
        ev.comp_fl(fl, other)

      def all(p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
        ev.all(fl)(p)

      def elem(a: A)(implicit B: Base[A]): Repr[Getter[S, Boolean]] =
        ev.elem(fl)(a)

      def getAll: Obs[S => List[A]] = ev.getAll(fl)
    }

    implicit def af_as_fl[Repr[_], Obs[_], S, A](
        af: Repr[Affine[S, A]])(implicit
        ev: Optica[Repr, Obs]): Repr[Fold[S, A]] =
      ev.as_fl(af)

    implicit def gt_as_fl[Repr[_], Obs[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev1: Optica[Repr, Obs], 
        ev2: Optica[Repr, Obs]): Repr[Fold[S, A]] =
      ev1.as_fl(ev2.as_af(gt))
  }
  
  object syntax extends Syntax
}

