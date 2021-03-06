package sx

import scalaz._
import Scalaz._
import collection.Seq

object Monoids extends App {
  object Step1 {
    trait Mon[A] {
      def zero: A
      def append(a1: A, a2: A): A
    }

    object IntMon extends Mon[Int] {
      def append(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    object StringM extends Mon[String] {
      def zero: String = ""

      def append(a1: String, a2: String): String = a1 + a2
    }

    def sumList[A](as: List[A], m: Mon[A]) = as.foldLeft(m.zero)(m.append)

    sumList(List(1, 2, 3), IntMon)
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

    def sumList[A](as: List[A])(implicit m: Mon[A]) =
      as.foldLeft(m.zero)(m.append)

    implicit def Tuple2Mon[A, B](implicit ma: Mon[A], mb: Mon[B])
    : Mon[(A, B)] = new Mon[(A, B)] {
      def append(a1: (A, B), a2: (A, B)): (A, B) =
        (ma.append(a1._1, a2._1), mb.append(a1._2, a2._2))

      def zero: (A, B) = (ma.zero, mb.zero)
    }

    sumList(List(1, 2, 3))
    println(sumList(List((1, "one"), (2, "two"))))
  }

  // Okay, lets use scalaz.Monoid instead
  object Step3 {


    ((1, "a") |+| (2, "b")) assert_===( (3, "ab"))
    
    List(1.some, 2.some, none[Int]).sumr assert_===(Some(3))

    Seq(0, 1, 2).foldMap(x => (x, x * x)) assert_===((3, 5))

    val fs: Seq[Int => Option[Int]] = Seq(
       (i: Int) => {println("f1"); if (i >= 0) Some(1) else None},
       (i: Int) => {println("f2"); if (i >= -1)
         Some(2)
       else
         None
       }
    )
    println("fSeq(1)")
    val result = fs.reverse.foldMap(_(0).fst)
    result.println

  }

  Step2
}