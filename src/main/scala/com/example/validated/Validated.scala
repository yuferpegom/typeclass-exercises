import cats._
import cats.implicits._

sealed trait Validated[+A] 

object Validated {

    case class Valid[+A](a: A) extends Validated[A]
    case class Invalid(errors: List[String]) extends Validated[Nothing]

    implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
        override def pure[A](x: A): Validated[A] = Valid(x) 
        override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = {
            (vf, va) match {
                case (Valid(f), Valid(a)) => Valid(f(a))
                case (Valid(f), Invalid(errors)) => Invalid(errors)
                case (Invalid(errors), Valid(a)) => Invalid(errors)
                case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
            }
            // map2(vf, va)((a, b) => a(b))
        }

        override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = {
            // (va, vb) match {
            //     case (Valid(a), Valid(b)) => Valid(f(a, b))
            //     case (Invalid(e), Valid(_)) => Invalid(e)
            //     case (Valid(_), Invalid(e)) => Invalid(e)
            //     case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
            // }
            val x: A => B => C = a => b => f(a, b) // f.curried
            val fun: Validated[A => B => C] = pure(x)
            ap(ap(fun)(va))(vb)
        }
        
        def tupled[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] = 
            map2(va, vb)((a, b) => (a, b) )
    }

    val optionApplicative: Applicative[Option] = new Applicative[Option] {
        override def pure[A](a: A): Option[A] = Option(a)

        override def ap[A, B](of: Option[A => B])(oa: Option[A]): Option[B] ={
            (of, oa) match {
                case (Some(f), Some(a)) => Option(f(a))
                case _ => None
            }
        }

    }

    val listApplicative: Applicative[List] = new Applicative[List] {
        override def pure[A](a: A): List[A] = List(a)

        override def ap[A, B](lf: List[A => B])(la: List[A]): List[B] ={
            (lf, la) match {
                case (f :: fs, a :: as) => (a :: as).fmap(f) ++ ap(fs)(a :: as) // fmap ensure we are using the cats version of map
                case _ => Nil
            }
        }

    }
}