package com.ming


object Monad_Exercise extends App {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Apply[F[_]] extends Functor[F] with Semigroupal[F] {
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???
  }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]
  }

  trait ApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](e: E): F[A]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A =>F[B]): F[B]
  }


  object MonadForOption extends Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case _ => None
    }

    override def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = fa match {
      case Some(a) => f.map(_(a))
      case _ => None
    }

    def ap2[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = f match {
      case Some(fab) => fa.map(fab)
      case _ => None
    }

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case _ => None
    }
  }

  println{
   MonadForOption.map(Some(1))(_ + 1)
  }
}

object Applicative_Exercise extends App {
  import cats._
  import cats.data._
  import cats.implicits._


  def fun1[F[_]: Applicative, E](value: Int)(implicit AE: ApplicativeError[F, E], EF: String => E): F[Int] = value match {
    case v if v<100 => Applicative[F].pure(v+1)
    case _ => AE.raiseError(EF("more than 100"))
  }

  Either.catchNonFatal(1/0)

}

object KleisliExercise extends App {
  import cats.data.Kleisli
  import cats.implicits._
  import scala.util.Try

  val fun1: String => Option[Int] = value => Try(value.toInt).toOption.orElse(None)
  val fun2: Int => Option[Double] = _ match {
    case 0 => None
    case value => Some(1.0/value)
  }

  val a = Kleisli(fun1) andThen Kleisli(fun2)

  a.run("one")
  a.run("0")
  a.run("10")
}

object exercise4 extends App {
  def fun1(value: Int): Either[String, Int] = {println("f1"); if (value<100) Left("less than 100") else Right(value)}
  def fun2(value: Int): Either[String, Int] = {println("f2"); if (value<200) Left("less than 200") else Right(value)}
  def fun3(value: Int): Either[String, Int] = {println("f3"); if (value<300) Left("less than 300") else Right(value)}

  val value = 10
  for {
    v1 <- fun1(value)
    v2 <- fun2(value)
    v3 <- fun3(value)
  } yield v3

  val fs = List(fun1 _, fun2 _, fun3 _)

  fs.map(_.apply(value))
    .reduceLeft((a, b) => if (a.isLeft) a else b)

  type E = Either[String, Int]
  import cats.implicits._
  fs.foldLeft(value.asRight[String])((a, b) => if (a.isLeft) a else b(value))
}

object ApplyMapN_Exercise extends App {

  import cats.implicits._

  case class Dog(v1: Int, v2: Int, v3: Int)

  val a = (1.some,2.some,3.some).mapN(Dog.apply)
  val b = (1.some,2.some,none[Int]).mapN(Dog.apply)
  val c = (1.valid[String], 2.valid[String], 3.valid[String]).mapN(Dog.apply)

  println(a)
  println(b)
  println(c)

  //ValidatedNel has Functor and Semigroupal instances so we can just use mapN.
  val d = (
    1.validNec[String],
    2.validNec[String],
    3.validNec[String]
  ).mapN (_ + _ + _)

  val e = (
    "error 1".invalidNec[Int],
    "error 2".invalidNec[Int],
    "error 3".invalidNec[Int]
  ).mapN (_ + _ + _)

  //There is Semigroup for String
  val f = (
    "error 1".invalid[Int],
    "error 2".invalid[Int],
    "error 3".invalid[Int]
  ).mapN (_ + _ + _)

  println(d)
  println(e)
  println(f)
}

object Validated_Exercise extends App {
  import cats.data._
  import cats._
  import cats.implicits._

  case class Person(name: String, age: Int, tel: String, email: String)

  object Person {
    def validateName(name: String): ValidatedNel[String, String] = Either.cond(
      name.matches("\\w+"),
      name,
      "invalid name"
    ).toValidatedNel

    def validateAge(age: Int) = Either.cond(
      age > 0,
      age,
      "invalid age"
    ).toValidatedNel

    def validateTel(tel: String) = Either.cond(
      tel.matches("\\d+"),
      tel,
      "invalid telephone"
    ).toValidatedNel

    def validateEmail(email: String) = Either.cond(
      email.matches("\\w+@\\w+\\.\\w+"),
      email,
      "invalid email"
    ).toValidatedNel
  }

  import Person._

  validateName("tom")
  validateName("tom1")
  validateName("tom1!@£")
  validateAge(10)
  validateAge(-1)
  validateTel("2123123")
  validateTel("2123123x")
  validateEmail("abc@aaa.com")
  validateEmail("abc@aaa")

  val a = (
    validateName("tom"),
    validateAge(-1),
    validateTel("2123123x"),
    validateEmail("abc@aaa")
  ).mapN(Person.apply)

  println(a)
}

object ShowTypeClass_Exercise extends App {
  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    // summon
    def apply[A](implicit instance: Show[A]): Show[A] = instance
  }

  object showSyntax {
    implicit val showString = new Show[String] {
      override def show(a: String): String = a
    }

    implicit class StringShow(s: String) {
      def show = Show[String].show(s)
    }
  }

  import showSyntax._

  val a = Show[String].show("hello") //summon
  val b = "world".show
  println(a, b)
}

object LCDDisplay_Exercise extends App {
  import cats._
  import cats.implicits._

  case class Display(line1: String, line2: String, line3: String)

  object Display {
    val digitalMapping = Map (
      1 -> Display("  ¦", "  ¦", "  ¦"),
      2 -> Display("⌜⎺⌝", "⌌-⌏", "¦_⌟")
    )

    implicit val displayShow = Show.show[Display](
      d => {d.line1} + "\n" + {d.line2} + "\n" + {d.line3}
    )

    implicit val displayMoniod = new Monoid[Display] {
      override def empty: Display = Display("", "", "")

      override def combine(x: Display, y: Display): Display =
        Display(x.line1 + " " + y.line1,
          x.line2 + " " + y.line2,
          x.line3 + " " + y.line3)
    }

    private def parse(s: String): Seq[Display] =
      s.map(d => digitalMapping(d.asDigit))

    def display(s: String) = Monoid[Display].combineAll(parse(s)).show
  }

  println(Display.display("21"))
}