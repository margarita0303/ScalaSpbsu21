package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{size, sum, fromSeq, sort}
import scala.math.Ordering.Int
import scala.math.Ordering.String

class MyGenericListTest extends AnyFunSuite {

  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert(MyNil.map((a: Int) => a * 2) == MyNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(MyNil))
    assert(sum(fromSeq(Seq(1,2,3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }

  test("sort") {
    assert(sort(MyNil: MyGenericList[Int]) == MyNil)
    assert(sort(fromSeq(Seq(0))) == fromSeq(Seq(0)))
    assert(sort(fromSeq(Seq(1,2,3))) == fromSeq(Seq(1,2,3)))
    assert(sort(fromSeq(Seq(10, 10, 10, 10, 10))) == fromSeq(Seq(10, 10, 10, 10, 10)))
    assert(sort(fromSeq(Seq(20, 6, 7, 6, 1))) == fromSeq(Seq(1, 6, 6, 7, 20)))
    assert(sort(fromSeq(Seq(3, 4, 5, 7, 6, 1, 2))) == fromSeq(Seq(1, 2, 3, 4, 5, 6, 7)))
    assert(sort(fromSeq(Seq(5, 785, 23, 2, 5, 1, 100))) == fromSeq(Seq(1, 2, 5, 5, 23, 100, 785)))
    assert(sort(fromSeq(Seq("abcd", "zzzz", "hello", "w")))== fromSeq(Seq("abcd", "hello", "w", "zzzz")))
    assert(sort(fromSeq(Seq("aaa", "ccc", "bbb"))) == fromSeq(Seq("aaa", "bbb", "ccc")))
  }
}
