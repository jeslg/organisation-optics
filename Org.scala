package dev.habla

import Optica.syntax._
import Project.syntax._

case class Department(dpt: String, employees: List[Employee])
case class Employee(emp: String, tasks: List[Task])
case class Task(tsk: String)

trait Project[Repr[_]] {
  
  def dpt(d: Repr[Department]): Repr[String]

  def employees(d: Repr[Department]): Repr[List[Employee]]

  def emp(e: Repr[Employee]): Repr[String]

  def tasks(e: Repr[Employee]): Repr[List[Task]]

  def tsk(t: Repr[Task]): Repr[String]

  def Department(
    dpt: Repr[String], 
    employees: Repr[List[Employee]]): Repr[Department]
  
  def Employee(
    emp: Repr[String],
    tasks: Repr[List[Task]]): Repr[Employee]

  def Task(tsk: Repr[String]): Repr[Task]
}

object Project {

  implicit object RProject extends Project[λ[x => x]] {

    def dpt(d: Department) = d.dpt

    def employees(d: Department) = d.employees

    def emp(e: Employee) = e.emp

    def tasks(e: Employee) = e.tasks

    def tsk(t: Task) = t.tsk

    def Department(dpt: String, employees: List[Employee]) =
      dev.habla.Department(dpt, employees)
    
    def Employee(emp: String, tasks: List[Task]) =
      dev.habla.Employee(emp, tasks)

    def Task(tsk: String) = dev.habla.Task(tsk)
  }

  trait Syntax {
  
    implicit class DepartmentOps[Repr[_]](
        d: Repr[Department])(implicit 
        PR: Project[Repr]) {
      def dpt = PR.dpt(d)
      def employees = PR.employees(d)
    }

    implicit class EmployeeOps[Repr[_]](
        e: Repr[Employee])(implicit 
        PR: Project[Repr]) {
      def emp = PR.emp(e)
      def tasks = PR.tasks(e)
    }

    implicit class TaskOps[Repr[_]](
        t: Repr[Task])(implicit 
        PR: Project[Repr]) {
      def tsk = PR.tsk(t)
    }
  }

  object syntax extends Syntax
}

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
      TL: TLinq[Repr],
      PR: Project[Repr]): Model[Repr] = new Model[Repr] {
    import TL._

    def departments = lam(identity)

    def dpt = lam(d => d.dpt)

    def employees = lam(d => d.employees)

    def emp = lam(e => e.emp)

    def tasks = lam(e => e.tasks)

    def tsk = lam(t => t.tsk)
  }
}

class Logic[Repr[_], Obs[_]](implicit
    O: Optica[Repr, Obs],
    M: Model[Repr]) {
  import O._, M._

  def expertiseFl(u: String): Repr[Fold[Org, String]] =
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt

  def expertise(u: String): Obs[Org => List[String]] = expertiseFl(u).getAll
}

