package dev.habla

import Optica._
import Model._
import Optica.syntax._
import Nested.syntax._

object Main extends App {

  // Runs a fold using the adapted relational state as source
  def run[Repr[_], Obs[_], A](
      fl: Repr[Fold[Org, A]])(implicit
      O: Optica[Repr, Obs],
      T: TLinq[Obs],
      S: Schema[Obs],
      N: Nested[Obs]): Obs[List[A]] =
    T.app(fl.getAll)(Schema.apply)

  implicit val ev0 = tLinqOptica[λ[x => x]]
  implicit val ev1 = tLinqModel[λ[x => x]]

  val logic = new Logic[Wrap[λ[x => x], ?], λ[x => x]]

  // Run expertise from logic
  assert(
    run[Wrap[λ[x => x], ?], λ[x => x], String](logic.expertiseFl("abstract")) ==
    List("Quality", "Research"))

  // Combines comprehensions with optica expressions. We could've used fold
  // fork composition here to implement this query, but it's not implemented in
  // the current Optica version. Despite that, there are many other queries
  // that we can't implement by means of optics (like `compose`), so this
  // connection turns out to be helpful.
  def expertsDepts[Repr[_], Obs[_]](
      u: String)(implicit
      O: Optica[Repr, Obs],
      M: Model[Repr],
      T: TLinq[Obs],
      N: Nested[Obs],
      S: Schema[Obs]): Obs[List[(String, String)]] = {
    import M._, T._, O._
    foreach(run(departments >>> filtered(employees.all((tasks >>> tsk).elem(u)))))(d =>
    foreach(d.employees)(e =>
    yields(tuple(d.dpt, e.emp))))
  }

  assert(expertsDepts[Wrap[λ[x => x], ?], λ[x => x]]("abstract") ==
    List("Research" -> "Cora", "Research" -> "Drew", "Research" -> "Edna"))
}

