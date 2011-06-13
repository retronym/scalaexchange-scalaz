package sx

import scalaz._
import Scalaz._

object TypeClass extends App {

  object Functors {
    // The Functor class [7] is the most basic and ubiquitous type class in the Haskell
    // libraries. A simple intuition is that a Functor represents a “container” of some
    // sort, along with the ability to apply a function uniformly to every element in the
    // container
    val is = List(1, 2, 3)

    // Type class instances are found, and wired together, with implicit resolution.
    // Scala looks for an implicit M[A] first in the current scope (including imports),
    // then in the companion object of M and A (and their super classes).
    //
    // We can use Predef.implicitly[X] to directly search for an implicit of type X
    // This is the same as
    val ListFunctor = implicitly[Functor[List]] // finds Functor.TraversableFunctor[List]

    // We can then call methods directly.
    ListFunctor.fmap(is, (i: Int) => i * 2)

    // Usually in Scalaz, we don't directly lookup or use methods on type classes, instead
    // we use methods in Identity, MA, or MAB.
    is ∘ (_ * 2)
    is.asMA.map(_ * 2)

    // Option is a Functor. Let's redefine it here.
    implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
      def fmap[A, B](r: Option[A], f: (A) => B): Option[B] = r map f
    }

    // Either[A, B], for some known A, can be treated like a functor.
    val either: Either[String, Int] = 1.right[String]
    val e2: Either[String, Int] = either ∘ (_ * 2)

    // How does this work?
    // We need to introduce a type alias with one type argument, over which the functor
    // is defined.
    type EitherStringA[A] = Either[String, A]
    val EitherStringAFunctor = implicitly[Functor[EitherStringA]]

    // The type lines above can be expressed in one with a 'type lambda'.
    val EitherStringAFunctor2 = implicitly[Functor[({type λ[α] = Either[String, α]})#λ]]
    EitherStringAFunctor.fmap(either, (i: Int) => i * 2)
  }

  object Pointed {
    // Given a Functor, the Pointed class represents the additional ability to put a
    // value into a “default context.” Often, this corresponds to creating a container with
    // exactly one element, but it is more general than that.

    // In Scalaz, Pointed combines Pure and Functor.

    // Lets make a datatype, and defined Pure/Functor (and hence, automatucally, Pointed).

    sealed abstract class E[A]
    case class E1[A](a: A) extends E[A]
    case class E2[A](a: A) extends E[A]

    implicit object EPure extends Pure[E] {
      def pure[A](a: => A): E[A] = E1(a)
    }
    implicit object EFunctor extends Functor[E] {
      def fmap[A, B](r: E[A], f: (A) => B): E[B] = r match {
        case E1(a) => E1(f(a))
        case E2(a) => E2(f(a))
      }
    }
    implicitly[Pointed[E]]
  }


  val x: Int = 1 |+| 1

  val o1 = 1.some
  val o2 = 2.some
  val n = none[Int]

  val result = o1 ⊹ o2 ⊹ n
  println(result)

  val os = Seq(o1, o2)
  println(os.sequence)

  val p1 = promise(1)
  val p2 = promise(2)
  val p3 = (p1 |@| p2) {
    _ + _
  }

  val p4 = p1 map (i => i * 2)
  val p5 = p1 map (i => promise(i * 2))

  pure[Option]

}