package com.example

case class Person(name: String, id: Int)
import Eq.instance

object Person {
  object Instances {
    // TODO #9: Define an Eq instance for Person comparing them by name
    //          Extra points: receive an implicit instance for String and use it
    implicit def personEqByName(implicit strEq: Eq[String]) = 
      instance[Person]((person1, person2) => strEq.eq(person1.name, person2.name))

    // TODO #10: Define an Eq instance for Person comparing them by id
    //           Extra points: receive an implicit instance for Int and use it
    implicit def personEqId(implicit integerEq: Eq[Int]) = 
      instance[Person]((person1, person2) => integerEq.eq(person1.id, person2.id))
  // TODO #6: Define an Eq instance for Person. Two persons are equal if both their names and ids are equal.
  //          Extra points: receive implicit instances for String and Int and use them
    implicit def personEq(implicit strEq: Eq[String], integerEq: Eq[Int]) = 
      instance[Person]((person1, person2) => strEq.eq(person1.name, person2.name) && integerEq.eq(person1.id, person2.id))
    }
}