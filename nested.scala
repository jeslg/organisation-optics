package dev.habla

case class Department(dpt: String, employees: List[Employee])
case class Employee(emp: String, tasks: List[Task])
case class Task(tsk: String)

// Nested schema that we use to (de)construct organisation types
trait Nested[Repr[_]] {
  
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

object Nested {

  implicit object RNested extends Nested[λ[x => x]] {

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
        N: Nested[Repr]) {
      def dpt = N.dpt(d)
      def employees = N.employees(d)
    }

    implicit class EmployeeOps[Repr[_]](
        e: Repr[Employee])(implicit 
        N: Nested[Repr]) {
      def emp = N.emp(e)
      def tasks = N.tasks(e)
    }

    implicit class TaskOps[Repr[_]](
        t: Repr[Task])(implicit 
        N: Nested[Repr]) {
      def tsk = N.tsk(t)
    }
  }

  object syntax extends Syntax
}

