package dev.habla

import Optica.syntax._

// Collects basic optica expressions and observations. Note that `expertsDepts`
// is defined in the main method instead, to avoid introducing TLinq
// dependencies here.
class Logic[Repr[_], Obs[_]](implicit
    O: Optica[Repr, Obs],
    M: Model[Repr]) {
  import O._, M._

  def expertiseFl(u: String): Repr[Fold[Org, String]] =
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt

  def expertise(u: String): Obs[Org => List[String]] = expertiseFl(u).getAll

  def expertsFl(u: String): Repr[Fold[Org, String]] =
    departments >>> 
      filtered(employees.all((tasks >>> tsk).elem(u))) >>>
      employees >>>
      emp

  def experts(u: String): Obs[Org => List[String]] = expertsFl(u).getAll

  def experts2Fl(u: String): Repr[Fold[Org, Employee]] =
    departments >>>
      filtered(employees.all((tasks >>> tsk).elem(u))) >>>
      employees

  def experts2(u: String): Obs[Org => List[Employee]] = experts2Fl(u).getAll
}

