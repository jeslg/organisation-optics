package dev.habla

import Optica._
import Model._
import Optica.syntax._
import Project.syntax._
import Schema.syntax._

case class DepartmentRel(dpt: String)
case class EmployeeRel(emp: String, dpt: String)
case class TaskRel(tsk: String, emp: String)

trait Schema[Repr[_]] {

  def table_department: Repr[List[DepartmentRel]]

  def table_employee: Repr[List[EmployeeRel]]

  def table_task: Repr[List[TaskRel]]

  def dpt(d: Repr[DepartmentRel]): Repr[String]

  def emp(e: Repr[EmployeeRel]): Repr[String]

  def dpt_fk(e: Repr[EmployeeRel]): Repr[String]

  def tsk(t: Repr[TaskRel]): Repr[String]

  def emp_fk(t: Repr[TaskRel]): Repr[String]
}

object Schema {
  
  implicit object RSchema extends Schema[λ[x => x]] {

    def table_department = List(
      DepartmentRel("Product"),
      DepartmentRel("Quality"),
      DepartmentRel("Research"),
      DepartmentRel("Sales"))

    def table_employee = List(
      EmployeeRel("Alex", "Product"),
      EmployeeRel("Bert", "Product"),
      EmployeeRel("Cora", "Research"),
      EmployeeRel("Drew", "Research"),
      EmployeeRel("Edna", "Research"),
      EmployeeRel("Fred", "Sales"))

    def table_task = List(
      TaskRel("build", "Alex"),
      TaskRel("build", "Bert"),
      TaskRel("abstract", "Cora"),
      TaskRel("build", "Cora"),
      TaskRel("design", "Cora"),
      TaskRel("abstract", "Drew"),
      TaskRel("design", "Drew"),
      TaskRel("abstract", "Edna"),
      TaskRel("call", "Edna"),
      TaskRel("design", "Edna"),
      TaskRel("call", "Fred"))

    def dpt(d: DepartmentRel) = d.dpt

    def emp(e: EmployeeRel) = e.emp

    def dpt_fk(d: EmployeeRel) = d.dpt

    def tsk(t: TaskRel) = t.tsk

    def emp_fk(t: TaskRel) = t.emp
  }

  trait Syntax {
  
    implicit class DepartmentRelOps[Repr[_]](
        d: Repr[DepartmentRel])(implicit 
        S: Schema[Repr]) {
      def dpt = S.dpt(d)
    }

    implicit class EmployeeRelOps[Repr[_]](
        e: Repr[EmployeeRel])(implicit 
        S: Schema[Repr]) {
      def emp = S.emp(e)
      def dpt_fk = S.dpt_fk(e)
    }

    implicit class TaskRelOps[Repr[_]](
        t: Repr[TaskRel])(implicit 
        S: Schema[Repr]) {
      def tsk = S.tsk(t)
      def emp_fk = S.emp_fk(t)
    }
  }

  object syntax extends Syntax
}

object Main extends App {

  def model[Repr[_]](implicit 
      T: TLinq[Repr],
      S: Schema[Repr],
      P: Project[Repr]): Repr[Org] = {
    import T._, S._, P.{Department, Employee, Task}
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
      P: Project[Obs]): Obs[List[A]] =
    T.app(fl.getAll)(model[Obs])

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
      P: Project[Obs],
      S: Schema[Obs]): Obs[List[(String, String)]] = {
    import M._, T._, O._
    foreach(run(departments >>> filtered(employees.all((tasks >>> tsk).elem(u)))))(d =>
    foreach(d.employees)(e =>
    yields(tuple(d.dpt, e.emp))))
  }

  assert(expertsDepts[Wrap[λ[x => x], ?], λ[x => x]]("abstract") ==
    List("Research" -> "Cora", "Research" -> "Drew", "Research" -> "Edna"))
}

