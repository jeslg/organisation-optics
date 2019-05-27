package dev.habla

import Optica._
import Nested.syntax._

trait Model[Repr[_]] {

  def departments: Repr[Fold[Org, Department]]

  def dpt: Repr[Getter[Department, String]]

  def employees: Repr[Fold[Department, Employee]]

  def emp: Repr[Getter[Employee, String]]

  def tasks: Repr[Fold[Employee, Task]]

  def tsk: Repr[Getter[Task, String]]
}

object Model {

  implicit def tLinqModel[Repr[_]](implicit 
      T: TLinq[Repr],
      N: Nested[Repr]): Model[Wrap[Repr, ?]] = new Model[Wrap[Repr, ?]] {
    import T._

    def departments = WFold(lam(identity))

    def dpt = WGetter(lam(d => d.dpt))

    def employees = WFold(lam(d => d.employees))

    def emp = WGetter(lam(e => e.emp))

    def tasks = WFold(lam(e => e.tasks))

    def tsk = WGetter(lam(t => t.tsk))
  }
}

