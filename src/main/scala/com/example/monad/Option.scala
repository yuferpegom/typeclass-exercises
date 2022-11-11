package option

import cats._
import cats.implicits._

// trait Option[+A] {
//     def map[B](f: A => B): Option[B] = {
//         flatMap(a => Some(f(a)))
//     }
//     def flatMap[B](f: A => Option[B]): Option[B] = {
//         this match {
//             case Some(a) => f(a)
//             case None => None
//         }
//     }
// } This works pretty nicely, lets use cats monad
// final case class Some[+A](a: A) extends Option[A] 
// case object None extends Option[Nothing]
sealed trait Option[+A]
object Option {
    final case class Some[+A](a: A) extends Option[A] 
    case object None extends Option[Nothing]

    implicit val monadOption: Monad[Option] = new Monad[Option] {

        override def pure[A](a: A): Option[A] = Some(a)
        override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = {
            a match {
                case Some(value) => f(value)
                case None => None
            }
        }
        override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ??? 
        override def map[A, B](a: Option[A])(f: A => B): Option[B] = flatMap(a)(value => pure(f(value)))
        override def flatten[A](aa: Option[Option[A]]): Option[A] = flatMap(aa)(identity)
    }

}

case class Person(name: String)
case class Account(balance: Double, owner: Person)
case class Transfer(source: Account, dest: Account, amount: Double)

trait AccountService {
    def findPersonByName(name: String): Option[Person]
    def findAccountByPerson(person: Person): Option[Account]
    def findLastTransferBySourceAccount(account: Account): Option[Transfer]
    def findLastTransferByPersonName(name: String): Option[Transfer] = {
        for {
            person <- findPersonByName(name)
            account <- findAccountByPerson(person)
            lastTransfer <- findLastTransferBySourceAccount(account)
        } yield lastTransfer
    }
}