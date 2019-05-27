package dev.habla

import Optica._
import Model._
import Optica.syntax._
import Nested.syntax._
import Schema.syntax._

object Main extends App {

  def model[Repr[_]](implicit 
      T: TLinq[Repr],
      S: Schema[Repr],
      N: Nested[Repr]): Repr[Org] = {
    import T._, S._, N.{Department, Employee, Task}
    foreach(table_department)(d =>
      yields(Department(d.dpt, foreach(table_employee)(e =>
        where(equal(d.dpt, e.dpt_fk))(
          yields(Employee(e.emp, foreach(table_task)(t =>
            where(equal(e.emp, t.emp_fk))(
              yields(Task(t.tsk)))))))))))
  }

  def run[Repr[_], Obs[_], A](
      fl: Repr[Fold[Org, A]])(implicit
      O: Optica[Repr, Obs],
      T: TLinq[Obs],
      S: Schema[Obs],
      N: Nested[Obs]): Obs[List[A]] =
    T.app(fl.getAll)(model)

  implicit val ev0 = tLinqOptica[λ[x => x]]
  implicit val ev1 = tLinqModel[λ[x => x]]

  val logic = new Logic[Wrap[λ[x => x], ?], λ[x => x]]

  assert(
    run[Wrap[λ[x => x], ?], λ[x => x], String](logic.expertiseFl("abstract")) ==
    List("Quality", "Research"))

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

