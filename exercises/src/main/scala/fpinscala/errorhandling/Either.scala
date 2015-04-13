package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a) 
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    // (this, b) match {
    //   case (Right(a), Right(b)) => Right(f(a, b))
    //   case (Left(e), _) => Left(e)
    //   case (_, Left(e)) => Left(e)
    // }
    for { a <- this; b1 <- b } yield f(a, b1)

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((x, y) => x.map2(y)(_ :: _))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val age: Int)

object Person {
  def mkName(name: String): Either[List[String], Name] =
    if (name == "" || name == null) Left(List("Name is empty"))
    else Right(new Name(name))

  def mkAge(age: Int): Either[List[String], Age] =
    if (age < 0) Left(List("Age is empty"))
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))  
}
