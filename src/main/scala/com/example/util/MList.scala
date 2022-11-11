package com.example.util

import cats._
import cats.implicits._

trait MList[+A] {
    def pure[B](b: B): MList[B] = MCons(b, MNil)
    def map[B](update: A => B): MList[B] = {
        // this match {
        //     case MCons(head, tail) => MCons(update(head), tail.map(update))
        //     case MNil => MNil
        // }
        // this.flatMap{ b: A => pure(update(b))  }
        ???
    }

    // def flatMap'

    // def flatMap[B](update: A => MList[B]): MList[B] = {
    //     this match {
    //         case MCons(head, tail) => MCons(update(head).head), tail.flatMap(update))
    //         case MNil => MNil
    //     }
    // }
}
case class MCons[+A](head: A, tail: MList[A]) extends MList[A] 
case object MNil extends MList[Nothing]

object MList {

    def append[A](a1: MList[A], a2: MList[A]): MList[A] =
    a1 match {
      case MNil => a2
      case MCons(h,t) => MCons(h, append(t, a2))
    }

    @annotation.tailrec 
    def foldLeft[A,B](l: MList[A], z: B)(f: (B, A) => B): B = {
      l match {
        case MNil => z
        case MCons(x, xs) => foldLeft(xs, f(z,x))(f) 
      }
    }

    def flatMap[A, B](list: MList[A])(update: A => MList[B]): MList[B] = {
        list match {
            case MCons(head, tail) => append(update(head), flatMap(tail)(update))
            case MNil => MNil
        }
    }

    trait Producer[+T] {
        // def produce(figure: T) error cov type cannot appear in contravarian position
        // def produce[A >: T](figure: A): T this would be the fix, ensures that A extends T
        def produce(): T
    }

sealed trait Polygon
case class Parallelogram() extends Polygon

val shape: Producer[Polygon] = new Producer[Parallelogram] {
 def produce(): Parallelogram = ???
}


trait Animal
case class Cat() extends Animal
case class Dog() extends Animal

trait Handler[-A] {
  def handle(a: A): Unit
}

val handler: Handler[Cat] = new Handler[Animal] {
    def handle(animal: Animal): Unit = ???
}
}