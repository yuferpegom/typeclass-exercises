package com.example

trait Eq[A] {
  // TODO #1: Define an 'eq' method that takes two A values as parameters, and returns a Boolean
  def eq(a: A, b: A): Boolean
}

object Eq {
  // TODO #2: Define the method 'apply' so we can summon instances from implicit scope
  def apply[A](implicit ev: Eq[A]) = ev

  // TODO #3: Define the method 'instance' so we can build instances of the Eq typeclass more easily.
  //          It should take as the only parameter a function of type (A, A) => Boolean
  def instance[A](f: (A, A) => Boolean) = new Eq[A] {
    override def eq(a: A, b: A) = f(a, b)
  }

  // TODO #4: Define an Eq instance for String
  implicit val stringEq = instance[String](_ == _)

  // TODO #5: Define an Eq instance for Int
  implicit val intEq = instance[Int](_ == _)


  // TODO #7: Provide a way to automatically derive instances for Eq[Option[A]] given that we have an implicit
  //          instance for Eq[A]

  def optionEq[A](implicit ev: Eq[A]): Eq[Option[A]] = new Eq[Option[A]] {

    override def eq(param1: Option[A], param2: Option[A]): Boolean = {
      (param1, param2) match {
        case (None, None) => true
        case (Some(_), None) => false
        case (None, Some(_)) => false
        case (Some(a), Some(b)) => ev.eq(a, b)
      }
    }
  }

  optionEq[String].eq(Some("a"), Some("b"))

  object Syntax {
    // TODO #8: Define a class 'EqOps' with a method 'eqTo' that enables the following syntax:
    //          "hello".eqTo("world")
    implicit class EqOps[A](a: A) {
      def eqTo(b: A)(implicit ev: Eq[A]): Boolean = {
        ev.eq(a, b)
      }
    }
    5.eqTo(5)
  }

}