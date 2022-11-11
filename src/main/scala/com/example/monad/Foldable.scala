package com.example.monad

import cats._
import cats.implicits._

// The same code is duplicated in MList so I can used somewhere else while keeping this the same

trait MList[+A] {

    // ALL OF THIS WORKS I'M NOW USING THE FOLDABLE TYPECLASS

    // def length(): Int = {
    //     // list match {
    //     //     case MCons(head, tail) => 1 + length(tail)
    //     //     case MNil => 0 
    //     // }
    //     foldRight(0)((_, acc) => 1 + acc)
    // }

    // def lengthFoldLeft(): Int = {
    //     // list match {
    //     //     case MCons(head, tail) => 1 + length(tail)
    //     //     case MNil => 0 
    //     // }
    //     foldLeft(0)((acc, _) => 1 + acc)
    // }

    // def foldRight[B](acc: B)(f: (A, B) => B): B = {
    //     this match {
    //         case MCons(head, tail) => f(head, foldRight(acc)(f)) 
    //         case MNil => acc 
    //     }
    // }

    // def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    //     this match {
    //         case MCons(head, tail) =>  foldLeft(f(acc, head))(f)
    //         case MNil => acc 
    //     }
    // }


}
case class MCons[+A](heaad: A, tail: MList[A]) extends MList[A]
case object MNil extends MList[Nothing]

object MList {

    def apply[A](elems: A*) = {
        elems.foldRight(mnil[A])((b, a) => mcons(b, a))
    }

    // This is how it looks like by using cats foldable

    implicit val listFoldable: Foldable[MList] = new Foldable[MList] {
        override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = {
            fa match {
                case MCons(head, tail) =>  foldLeft(tail, f(b, head))(f)
                case MNil => b 
            }
        }
        
        override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
            fa match {
                case MCons(head, tail) => f(head, Eval.defer(foldRight(fa, lb)(f))) 
                case MNil => lb 
            }
        }
        
    }

    def mnil[A]: MList[A] = MNil
    def mcons[A](head: A, tail: MList[A]): MList[A] = MCons(head, tail)

    def sum(ints: MList[Int]): Int = {
        // ints match {
        //     case MCons(head, tail) => head + sum(tail)
        //     case MNil => 0
        // }
        // ints.foldRight(0)(_ + _) This is using the implementation in MList
        ints.foldRight(Eval.Zero)((acc, b) => b.map(x => x + acc)).value // Using cats
    }

    def sumFoldLeft(ints: MList[Int]): Int = {
        // ints match {
        //     case MCons(head, tail) => head + sum(tail)
        //     case MNil => 0
        // }
        // ints.foldLeft(0)(_ + _) // using the imple in the MList
        ints.foldLeft(0)(_ + _) // using foldable type class
    }

    def filterPositive(list: MList[Int]): MList[Int] = {
        // list match {
        //     case MCons(head, tail) => if(head >= 0) MCons(head, filterPositive(tail)) else filterPositive(tail)
        //     case MNil => MNil
        // }
        // list.foldRight(MNil: MList[Int])((head, acc) => if(head >= 0) MCons(head, acc) else acc) using my own impl in MList
        // list.foldLeft(mnil[Int])((acc, head) => if(head >= 0) mcons(head, acc) else acc) This is showing results in incorrect order we might not want that
        list.foldRight(Eval.now(mnil[Int]))((head, acc) => if(head > 0) Eval.now(mcons(head, acc.value)) else acc).value // using foldable typeclass
    }

    def length[A](list: MList[A]): Int = {
        list.foldLeft(0)((acc, _) => 1 + acc)
    }

    def foldMap[A, B, F[_]: Foldable](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B = {
        fa.foldLeft(M.empty)((b, a) => M.combine(b, f(a)))
    }

    def foldMapOtherWay[A, B, F[_]](fa: F[A])(f: A => B)(implicit M: Monoid[B], Fold: Foldable[F]): B = {
        fa.foldLeft(M.empty)((b, a) => M.combine(b, f(a)))
    }

    def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] = 
        fa.foldLeft(Option.empty[A])((acc, a) => if(p(a)) Some(a) else acc)

    def exist[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = 
        // fa.foldLeft(false)((acc, a) => if(p(a)) true else acc)
        find(fa)(p).nonEmpty

    def toList[F[_]: Foldable, A](fa: F[A]): MList[A] = 
        // fa.foldLeft(mnil[A])((acc, a) => mcons(a, acc)) // Wrong order
        fa.foldRight(Eval.now(mnil[A]))((a, acc) => Eval.now(mcons(a, acc.value))).value

    def forAll[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = 
        // fa.foldLeft(false)((acc, a) => if(p(a)) acc else false)
        exist(fa)(a => !p(a))
        

}