package sx

import scalaz._
import Scalaz._


object Applicatives extends App {
  def foo(a: Int, b: String) = List.fill(a)(b).intersperse(".").collapse

  foo(10, "x") assert_=== "x.x.x.x.x.x.x.x.x.x"

  (5.some |@| "x".some)(foo) assert_=== Some("x.x.x.x.x")

  (5.some |@| none[String])(foo) assert_=== none[String]

}