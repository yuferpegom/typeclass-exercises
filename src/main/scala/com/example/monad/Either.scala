// import cats._
// import cats.implicits._

// object MonadEither {

//     implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
//         override def pure[A](a: A): Either[E, A] = Right(a)
//         override def flatMap[A, B](a: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???
//         override def tailRecM[A, B](a: A)(f: A => Either[E, B]): Either[E, B] = ??? 
//     }


  
// }
// Kind projector is not working