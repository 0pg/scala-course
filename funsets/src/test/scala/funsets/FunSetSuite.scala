package funsets

import org.junit.Assert.assertFalse
import org.junit._

import scala.annotation.tailrec

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `forall predicate even then only contains even number`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assertFalse(forall(s, x => x % 2 == 0))
    }
  }

  @Test def `exist not have 2`: Unit = {
    val s = union(union(singletonSet(1), singletonSet(3)), singletonSet(4))
    assertFalse(exists(s, x => x == 2))
  }

  @Test def `forall only even numbers`: Unit = {
    val testSet = makeTestSet(singletonSet(0), 1, x => x % 2 == 0)
    assert(forall(testSet, x => x % 2 == 0))
  }

  @Test def `odd doubling only contains even numbers`: Unit = {
    val init = makeTestSet(singletonSet(1), 2, x => x % 2 != 0)
    val testSet = map(init, x => x * 2)
    assert(forall(testSet, x => x % 2 == 0))
  }

  @tailrec
  private def makeTestSet(s: FunSet, num: Int, p: Int => Boolean): FunSet = {
    if (num > bound) s
    else if (p(num)) union(s, singletonSet(num))
    else makeTestSet(s, num + 1, p)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
