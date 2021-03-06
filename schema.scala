package dev.habla

case class DepartmentRel(dpt: String)
case class EmployeeRel(emp: String, dpt: String)
case class TaskRel(tsk: String, emp: String)

// Provides ad hoc table primitives and relational datastructure projections
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
  import syntax._

  // Adapts the relational model into a nested representation
  def apply[Repr[_]](implicit 
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
  
  // Provides straightforward projections and data to play with
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

