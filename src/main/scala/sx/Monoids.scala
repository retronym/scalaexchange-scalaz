package sx

import scalaz._
import Scalaz._

object Monoids extends App {
  object Step1 {
    trait Mon[A] {
      def zero: A
      def append(a1: A, a2: A): A
    }

    object IntM extends Mon[Int] {
      def append(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    object StringM extends Mon[String] {
      def zero: String = ""

      def append(a1: String, a2: String): String = a1 + a2
    }

    def sumList[A](as: List[A], m: Mon[A]) = as.foldLeft(m.zero)(m.append)

    sumList(List(1, 2, 3), IntM)
  }

  // Use implicits to wire the correct Mon into sumList.
  object Step2 {
    trait Mon[A] {
      def zero: A
      def append(a1: A, a2: A): A
    }

    implicit object IntM extends Mon[Int] {
      def append(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    implicit object StringM extends Mon[String] {
      def zero: String = ""

      def append(a1: String, a2: String): String = a1 + a2
    }

    def sumList[A](as: List[A])(implicit m: Mon[A]) = as.foldLeft(m.zero)(m.append)

    sumList(List(1, 2, 3))
  }

  // Okay, lets use scalaz.Monoid instead
  object Step3 {

    ((1, "a") |+| (2, "b")) assert_===( (3, "ab"))
    
    List(1.some, 2.some, none[Int]).sumr assert_===(Some(3))

    Seq(0, 1, 2).foldMap(x => (x, x * x)) assert_===((3, 5))
  }

  Step3
}