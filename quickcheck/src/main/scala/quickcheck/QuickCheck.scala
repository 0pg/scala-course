package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert two elements and find min then returns smaller one") = forAll { (n1: Int, n2: Int) =>
    val h = insert(n2, insert(n1, empty))
    if (n1 < n2) findMin(deleteMin(h)) == n2 else findMin(deleteMin(h)) == n1
  }

  property("insert and delete then heap is empty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("delete min and insert from min must equals") = forAll { a: Int =>
    val b = a / 2
    val h = insert(b + 1, insert(b, insert(b + 2, empty)))
    val rh = deleteMin(h)
    rh == insert(b + 2, insert(b + 1, empty))
  }

  property("find and delete always sorted") = {
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val h2 = deleteMin(h)
        if (isEmpty(h2)) true
        else {
          val min2 = findMin(h2)
          if (min > min2) false
          else isSorted(h2)
        }
      }

    forAll { h: H =>
      isSorted(h)
    }
  }

  property("melded heap always has same min value compared to sources") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
    else if (isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else {
      if (findMin(h1) > findMin(h2)) findMin(meld(h1, h2)) == findMin(h2)
      else findMin(meld(h1, h2)) == findMin(h1)
    }
  }
}
